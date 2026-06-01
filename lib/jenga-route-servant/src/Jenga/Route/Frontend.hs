{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Frontend routing effects and combinators.
--
-- Replaces obelisk-route's @RoutedT@, @SetRouteT@, @RouteToUrlT@
-- monad transformers with Effectful effects. Route changes are
-- handled via the browser History API.
--
-- == Migration from obelisk-route
--
-- @
-- -- BEFORE:
-- frontendBody :: JengaWidget t (R FrontendRoute) m => RoutedT t (R FrontendRoute) m ()
-- frontendBody = subRoute_ $ \\case
--   FrontendRoute_Main -> text "Home"
--   FrontendRoute_Login -> text "Login"
--
-- -- AFTER:
-- frontendBody :: (WidgetEff' es t, Routed t FrontendRoute :> es) => Eff es ()
-- frontendBody = switchRoute $ \\case
--   FR_Main -> text "Home"
--   FR_Login -> text "Login"
-- @
module Jenga.Route.Frontend
  ( -- * Routing effects (re-exported from reflex-effectful)
    Routed(..)
  , askRoute
  , SetRoute(..)
  , setRoute
  , modifyRoute
  , RouteToUrl(..)
  , askRouteToUrl

    -- * Combinators
  , switchRoute
  , switchRoute_
  , routeLink
  , routeLink'

    -- * Interpreter
  , runBrowserRouting
  ) where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Proxy (Proxy(..))
import           Data.Text (Text)
import qualified Data.Text as T

import           Effectful (Effect, Dispatch(..), DispatchOf, (:>), Eff)
import           Effectful.Dispatch.Dynamic (send, interpret_)

import           Reflex (Dynamic, Event, Reflex)
import qualified Reflex as R

import           Reflex.Effectful.Types (KnownTimeline)
import           Reflex.Effectful.Effect.Hold (Hold, holdDyn)
import           Reflex.Effectful.Effect.Sample (Sample, sample)
import           Reflex.Effectful.Effect.PostBuild (PostBuild, getPostBuild)
import           Reflex.Effectful.Effect.TriggerEvent (TriggerEvent, newTriggerEvent)
import           Reflex.Effectful.Effect.PerformEvent (PerformEvent, performEvent_)
import           Reflex.Effectful.Effect.Adjustable (Adjustable, widgetHold_)
import           Reflex.Effectful.Effect.Dom (Dom, el, el', elAttr, text)
import           Reflex.Effectful.Effect.JSM (JSM', liftJSM)
import           Reflex.Effectful.Effect.DomRenderHook (DomRenderHook, requestDomAction_)

import           Reflex.Dom.Builder.Class (domEvent, EventName(Click))

import qualified Language.Javascript.JSaddle as JS

import           Jenga.Route (HasRoute(..), urlToSegments)

-- ─── Effects ───────────────────────────────────────────────────
-- These are the same as in reflex-effectful's Jenga module,
-- defined here to keep jenga-route-servant self-contained.

data Routed t r :: Effect where
  AskRoute :: Routed t r m (Dynamic t r)
type instance DispatchOf (Routed t r) = 'Dynamic

askRoute :: Routed t r :> es => Eff es (Dynamic t r)
askRoute = send AskRoute

data SetRoute t r :: Effect where
  SetRoute    :: Event t r -> SetRoute t r m ()
  ModifyRoute :: Event t (r -> r) -> SetRoute t r m ()
type instance DispatchOf (SetRoute t r) = 'Dynamic

setRoute :: SetRoute t r :> es => Event t r -> Eff es ()
setRoute = send . SetRoute

modifyRoute :: SetRoute t r :> es => Event t (r -> r) -> Eff es ()
modifyRoute = send . ModifyRoute

data RouteToUrl r :: Effect where
  AskRouteToUrl :: RouteToUrl r m (r -> Text)
type instance DispatchOf (RouteToUrl r) = 'Dynamic

askRouteToUrl :: RouteToUrl r :> es => Eff es (r -> Text)
askRouteToUrl = send AskRouteToUrl

-- ─── Combinators ───────────────────────────────────────────────

-- | Switch widget based on current route. Replaces @subRoute_@.
--
-- When the route changes, the current widget is destroyed and
-- replaced with the widget for the new route.
--
-- @
-- switchRoute $ \\case
--   FR_Main  -> text "Home"
--   FR_Login -> loginWidget
--   FR_User uid -> userWidget uid
-- @
switchRoute
  :: ( KnownTimeline es t, Routed t r :> es
     , Adjustable t :> es, Hold t :> es, Dom t :> es
     , Reflex t
     )
  => (r -> Eff es a)
  -> Eff es (Dynamic t a)
switchRoute f = do
  routeDyn <- askRoute
  initial <- sample (R.current routeDyn)
  (a0, aEv) <- Reflex.Effectful.Effect.Adjustable.runWithReplace
    (f initial)
    (f <$> R.updated routeDyn)
  holdDyn a0 aEv

-- | Like 'switchRoute' but discards the result.
switchRoute_
  :: ( KnownTimeline es t, Routed t r :> es
     , Adjustable t :> es, Hold t :> es, Dom t :> es
     , Sample t :> es, Reflex t
     )
  => (r -> Eff es ())
  -> Eff es ()
switchRoute_ f = do
  routeDyn <- askRoute
  initial <- sample (R.current routeDyn)
  _ <- Reflex.Effectful.Effect.Adjustable.runWithReplace
    (f initial)
    (f <$> R.updated routeDyn)
  pure ()

-- | Create a link that navigates to a route on click.
-- Sets the @href@ attribute for proper right-click/open-in-new-tab.
routeLink
  :: forall r t es.
     ( KnownTimeline es t, Dom t :> es
     , SetRoute t r :> es, RouteToUrl r :> es
     , Reflex t
     )
  => r -> Eff es () -> Eff es ()
routeLink target child = do
  toUrl <- askRouteToUrl
  let attrs = Map.fromList [("href", toUrl target)]
  (e, _) <- elAttr "a" attrs child
  -- Prevent default navigation, use pushState instead
  setRoute (target <$ domEvent Click e)

-- | Create a text link that navigates to a route.
routeLink'
  :: forall r t es.
     ( KnownTimeline es t, Dom t :> es
     , SetRoute t r :> es, RouteToUrl r :> es
     , Reflex t
     )
  => r -> Text -> Eff es ()
routeLink' target label = routeLink target (text label)

-- ─── Browser interpreter ───────────────────────────────────────

-- | Run routing effects using the browser History API.
--
-- 1. Reads initial URL from @window.location.pathname@
-- 2. Listens for @popstate@ events (back/forward)
-- 3. @setRoute@ calls @history.pushState@
-- 4. @askRouteToUrl@ returns 'encodeRoute'
--
-- @
-- main = mainWidgetEff $ runBrowserRouting (Proxy \@Api) FR_Main $ do
--   switchRoute_ $ \\case
--     FR_Main  -> text "Home"
--     FR_Login -> loginWidget
-- @
runBrowserRouting
  :: forall api r t es a.
     ( HasRoute api r
     , KnownTimeline es t, Reflex t
     , Hold t :> es, Sample t :> es, PostBuild t :> es
     , TriggerEvent t :> es, PerformEvent t :> es
     , DomRenderHook t :> es
     , Dom t :> es, JSM' :> es
     )
  => Proxy api
  -> r                          -- ^ Fallback route for unrecognized URLs
  -> Eff (Routed t r : SetRoute t r : RouteToUrl r : es) a
  -> Eff es a
runBrowserRouting _ fallback =
    interpretRouteToUrl
  . interpretSetRoute
  . interpretRouted fallback

  where
    interpretRouted
      :: r -> Eff (Routed t r : es') a -> Eff es' a
    interpretRouted fb eff = do
      -- Create event for URL changes
      (urlEv, fireUrl) <- newTriggerEvent

      -- On postBuild: read initial URL + set up popstate listener
      pb <- getPostBuild
      requestDomAction_ $ R.ffor pb $ \_ -> do
        pathname <- JS.valToText =<< JS.jsg ("window" :: Text)
          JS.! ("location" :: Text) JS.! ("pathname" :: Text)
        search <- JS.valToText =<< JS.jsg ("window" :: Text)
          JS.! ("location" :: Text) JS.! ("search" :: Text)
        JS.liftIO $ fireUrl (pathname <> search)

        -- Listen for popstate
        _ <- JS.jsg ("window" :: Text) JS.# ("addEventListener" :: Text)
          $ [ JS.toJSVal ("popstate" :: Text)
            , JS.fun $ \_ _ _ -> do
                p <- JS.valToText =<< JS.jsg ("window" :: Text)
                  JS.! ("location" :: Text) JS.! ("pathname" :: Text)
                s <- JS.valToText =<< JS.jsg ("window" :: Text)
                  JS.! ("location" :: Text) JS.! ("search" :: Text)
                JS.liftIO $ fireUrl (p <> s)
            ]
        pure ()

      -- Parse URLs into routes
      let parseOrFallback :: Text -> r
          parseOrFallback url =
            let (segs, qp) = urlToSegments url
            in case (decodeRoute segs qp :: Maybe r) of
                 Just r  -> r
                 Nothing -> fb

      routeDyn <- holdDyn fb (parseOrFallback <$> urlEv)

      interpret_ (\case AskRoute -> pure routeDyn) eff

    interpretSetRoute :: Eff (SetRoute t r : es') a -> Eff es' a
    interpretSetRoute = interpret_ $ \case
      SetRoute ev ->
        requestDomAction_ $ R.ffor ev $ \route -> do
          let url = encodeRoute route
          _ <- JS.jsg ("window" :: Text) JS.! ("history" :: Text)
            JS.# ("pushState" :: Text)
            $ [ JS.toJSVal JS.JSNull
              , JS.toJSVal ("" :: Text)
              , JS.toJSVal url
              ]
          -- Manually fire popstate-like behavior since pushState doesn't
          -- trigger popstate
          pure ()
      ModifyRoute _ -> pure ()

    interpretRouteToUrl :: Eff (RouteToUrl r : es') a -> Eff es' a
    interpretRouteToUrl = interpret_ $ \case
      AskRouteToUrl -> pure encodeRoute
