{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Frontend rendering system using Effectful + Servant routing.
--
-- Replaces @Jenga.Frontend@ from obelisk. Provides:
--
-- * 'Frontend' record holding head/body widgets
-- * 'runFrontend' — live rendering (hydration or immediate)
-- * 'renderFrontendHtml' — SSR (static HTML for initial page load)
--
-- The same frontend code runs in two modes:
--
-- 1. __SSR__ (server): 'renderFrontendHtml' runs the widget through
--    'renderStatic', producing HTML with injected configs. The browser
--    receives pre-rendered DOM.
--
-- 2. __Live__ (browser): 'runFrontend' runs the widget via 'mainWidgetEff'.
--    If hydrating, it attaches to existing SSR'd DOM. If immediate
--    (jsaddle-warp dev), it builds DOM from scratch.
module Jenga.Frontend
  ( -- * Frontend type
    Frontend(..)
  , FrontendMode(..)

    -- * Running the frontend
  , runFrontend

    -- * Server-side rendering
  , renderFrontendHtml

    -- * Utilities
  , removeHTMLConfigs
  , baseTag
  ) where

import           Control.Monad (when, void)
import           Data.ByteString (ByteString)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           System.Info (os)

import           Effectful (Eff, IOE, (:>))

import qualified Reflex.Dom.Core as RD
import           GHCJS.DOM.Types (JSM)
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Document as DOM
import qualified GHCJS.DOM.Node as DOM
import qualified GHCJS.DOM.NodeList as DOM
import qualified GHCJS.DOM.ParentNode as DOM

import           Reflex.Effectful
import           Reflex.Effectful.Run (WidgetEff')

import           Jenga.Route (HasRoute(..))
import           Jenga.Route.Frontend

-- ─── Frontend type ─────────────────────────────────────────────

-- | The frontend of a Jenga application.
--
-- Holds the head and body widgets as effectful computations.
-- Both receive routing, configs, and cookies via effects.
--
-- Compare with obelisk's @Frontend (R route)@ which used
-- @RoutedT t route m ()@ — here we use @Eff es ()@ with
-- the effects providing the same capabilities.
data Frontend api r = Frontend
  { _frontend_head :: !(forall es t.
      ( WidgetEff' es t
      , Routed t r :> es
      , SetRoute t r :> es
      , RouteToUrl r :> es
      ) => Eff es ())
  , _frontend_body :: !(forall es t.
      ( WidgetEff' es t
      , Routed t r :> es
      , SetRoute t r :> es
      , RouteToUrl r :> es
      ) => Eff es ())
  }

-- | Controls how the frontend renders.
data FrontendMode = FrontendMode
  { _frontendMode_hydrate :: !Bool
    -- ^ Hydrate existing SSR'd DOM instead of building from scratch
  , _frontendMode_adjustRoute :: !Bool
    -- ^ Use hash-based routing (for platforms without ambient URLs)
  } deriving (Show, Eq)

-- ─── Live rendering ────────────────────────────────────────────

-- | Run the frontend live in the browser.
--
-- In production (GHCJS/WASM): hydrates SSR'd HTML.
-- In dev (jsaddle-warp): builds DOM from scratch.
--
-- @
-- main :: IO ()
-- main = runFrontend (Proxy \@FrontendPages) FR_Main configs frontend
-- @
runFrontend
  :: forall api r.
     HasRoute api r
  => Proxy api
  -> r                          -- ^ Fallback route
  -> Map Text ByteString        -- ^ Configs
  -> Frontend api r
  -> JSM ()
runFrontend proxy fallback configs frontend = do
  let mode = FrontendMode
        { _frontendMode_hydrate =
#if defined(ghcjs_HOST_OS) || defined(wasm32_HOST_ARCH)
            True
#else
            False
#endif
        , _frontendMode_adjustRoute =
#if defined(ghcjs_HOST_OS) || defined(wasm32_HOST_ARCH)
            False
#else
            True
#endif
        }
  when (_frontendMode_hydrate mode) removeHTMLConfigs
  -- TODO: Run through mainWidgetEff with routing, configs, cookies effects.
  -- The full implementation needs:
  -- 1. mainWidget (or mainHydrationWidget for hydration mode)
  -- 2. runBrowserRouting to set up Routed/SetRoute/RouteToUrl
  -- 3. Configs effect interpreter (from the configs map)
  -- 4. Cookies effect interpreter (from document.cookie)
  --
  -- For now, delegate to mainWidgetEff + runBrowserRouting:
  mainWidgetEff $ runBrowserRouting proxy fallback $ do
    _frontend_body frontend

-- ─── Server-side rendering (SSR) ──────────────────────────────

-- | Render the frontend to static HTML for server-side rendering.
--
-- This runs the frontend widget through reflex-dom's 'renderStatic',
-- producing a ByteString of HTML. The result includes:
--
-- * @\<!DOCTYPE html\>@ prefix
-- * Injected configs as base64 @\<script\>@ tags (read by frontend JS)
-- * Pre-rendered DOM (hydrated by the frontend on load)
-- * Script tag for the frontend JS/WASM bundle
--
-- @
-- html <- renderFrontendHtml configs cookies encodeRoute currentRoute
--           frontend headExtra bodyExtra
-- -- Serve html as the HTTP response
-- @
renderFrontendHtml
  :: MonadIO m
  => Map Text ByteString        -- ^ Configs
  -> [(ByteString, ByteString)] -- ^ Cookies
  -> (r -> Text)                -- ^ Route encoder
  -> r                          -- ^ Current route
  -> Frontend api r
  -> (forall t m'. (RD.DomBuilder t m', RD.PostBuild t m', RD.MonadHold t m') => m' ())
    -- ^ Extra head content (e.g. GHCJS preload link)
  -> (forall t m'. (RD.DomBuilder t m', RD.PostBuild t m', RD.MonadHold t m') => m' ())
    -- ^ Extra body content (e.g. GHCJS script tag)
  -> m ByteString
renderFrontendHtml configs cookies urlEnc route frontend headExtra bodyExtra = do
  -- TODO: Full SSR implementation.
  -- This needs to:
  -- 1. Run renderStatic (from reflex-dom) to produce static HTML
  -- 2. Run the Eff frontend through a static interpreter
  --    (StaticDomBuilderT instead of HydrationDomBuilderT)
  -- 3. Inject configs into head
  -- 4. Set the route to the requested route
  --
  -- The challenge: our MVar bridge runs the Eff in a forked thread
  -- communicating with Widget via MVar. For SSR, the Widget thread
  -- uses StaticDomBuilderT + renderStatic. The Eff thread sends
  -- DOM operations which get rendered to static HTML.
  --
  -- For now, produce a minimal HTML shell:
  liftIO $ pure $ mconcat
    [ "<!DOCTYPE html><html><head>"
    , mconcat [ "<script type=\"text/plain\" data-jenga-executable-config-inject-key=\""
              <> T.encodeUtf8 k <> "\">"
              <> encodeBase64 v <> "</script>"
              | (k, v) <- Map.toList configs ]
    , "</head><body>"
    , "<div id=\"app\">Loading...</div>"
    , "</body></html>"
    ]
  where
    encodeBase64 :: ByteString -> ByteString
    encodeBase64 = id -- TODO: proper base64 encoding

-- ─── Utilities ─────────────────────────────────────────────────

-- | Remove injected config elements from the DOM.
-- Called after hydration to clean up SSR artifacts.
removeHTMLConfigs :: JSM ()
removeHTMLConfigs = void $ do
  mdoc <- DOM.currentDocument
  case mdoc of
    Nothing -> pure ()
    Just doc -> do
      mhead <- DOM.getHead doc
      case mhead of
        Nothing -> pure ()
        Just hd -> do
          es <- DOM.querySelectorAll hd
            ("[data-jenga-executable-config-inject-key]" :: Text)
          len <- DOM.getLength es
          when (len > 0) $ do
            nodes <- traverse (DOM.item es) [0..len-1]
            mapM_ (\mn -> case mn of
              Just n -> DOM.removeChild_ hd n
              Nothing -> pure ()) nodes

-- | Emit a @\<base href=\"/\"\>@ tag. Omitted on iOS.
baseTag :: DomEff' es t => Eff es ()
baseTag =
  if os == "ios"
    then blank
    else elAttr "base" (Map.singleton "href" "/") blank
