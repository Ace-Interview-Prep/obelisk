{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Servant-based route encoding/decoding for Jenga.
--
-- Replaces obelisk-route's @Encoder@ chain with Servant type-level
-- combinators. Route validity is checked at compile time instead
-- of startup.
--
-- == Migration from obelisk-route
--
-- @
-- -- BEFORE (obelisk-route):
-- data FrontendRoute a where
--   FrontendRoute_Main  :: FrontendRoute ()
--   FrontendRoute_Login :: FrontendRoute ()
--   FrontendRoute_User  :: FrontendRoute Int
--
-- checkedEncoder = checkEncoder $ mkFullRouteEncoder
--   (FullRoute_Frontend (JengaRoute_App FrontendRoute_Main) :/ ())
--   backendSegment
--   (\\case
--     FrontendRoute_Main  -> PathSegment "main" (unitEncoder mempty)
--     FrontendRoute_Login -> PathSegment "login" (unitEncoder mempty)
--     FrontendRoute_User  -> PathSegment "user" singlePathSegmentEncoder)
--
-- -- AFTER (jenga-route-servant):
-- type FrontendPages =
--        "main"  :> Page
--   :<|> "login" :> Page
--   :<|> "user"  :> Capture "uid" Int :> Page
--
-- data FrontendRoute = FR_Main | FR_Login | FR_User Int
--
-- instance HasRoute FrontendPages FrontendRoute where
--   encodeRoute FR_Main     = safeLink api (Proxy \@("main" :> Page))
--   encodeRoute FR_Login    = safeLink api (Proxy \@("login" :> Page))
--   encodeRoute (FR_User n) = safeLink api (Proxy \@("user" :> Capture "uid" Int :> Page)) n
--   decodeRoute = parseRoute (Proxy \@FrontendPages)
-- @
module Jenga.Route
  ( -- * Core types (kept for compatibility)
    R
  , pattern (:/)
  , pattern (:.)
  , PageName

    -- * Route hierarchy
  , FullRoute(..)
  , JengaRoute(..)
  , ResourceRoute(..)

    -- * Servant-based encoding
  , Page
  , HasRoute(..)
  , renderRoute
  , renderFrontendRoute
  , renderBackendRoute

    -- * URL parsing
  , parseUrlSegments
  , urlToSegments

    -- * Re-exports
  , Proxy(..)
#if !defined(wasm32_HOST_ARCH) && !defined(javascript_HOST_ARCH)
    -- * Re-exports from Servant (native only)
  , (:<|>)(..)
  , (:>)
  , Capture
  , QueryParam
#endif
  ) where

import           Data.Dependent.Sum (DSum(..))
import           Data.Functor.Identity (Identity(..))
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Proxy (Proxy(..))
import           Data.Text (Text)
import qualified Data.Text as T
#if !defined(wasm32_HOST_ARCH) && !defined(javascript_HOST_ARCH)
import           Servant.API ((:<|>)(..), (:>), Capture, QueryParam)
#endif

-- | Existential wrapper for a route GADT. Kept for compatibility
-- with existing code that uses @R FrontendRoute@.
type R f = DSum f Identity

-- | Convenience pattern for constructing/matching @R@ values.
pattern (:/) :: f a -> a -> R f
pattern a :/ b = a :=> Identity b
{-# COMPLETE (:/) #-}
infixr 5 :/

-- | Pair synonym kept for compatibility.
type (:.) = (,)
pattern (:.) :: a -> b -> a :. b
pattern a :. b = (a, b)
infixr 5 :.

-- | Path segments + query parameters. Same type as obelisk-route.
type PageName = ([Text], Map Text (Maybe Text))

-- ─── Route hierarchy ───────────────────────────────────────────
-- Kept structurally identical to obelisk-route for compatibility.

data FullRoute br fr a where
  FullRoute_Backend  :: br a -> FullRoute br fr a
  FullRoute_Frontend :: JengaRoute fr a -> FullRoute br fr a

data JengaRoute fr a where
  JengaRoute_App      :: fr a -> JengaRoute fr a
  JengaRoute_Resource :: ResourceRoute a -> JengaRoute fr a

data ResourceRoute a where
  ResourceRoute_Static     :: ResourceRoute [Text]
  ResourceRoute_Ghcjs      :: ResourceRoute [Text]
  ResourceRoute_JSaddleWarp :: ResourceRoute [Text]
  ResourceRoute_Version    :: ResourceRoute ()

-- ─── Servant Page marker ───────────────────────────────────────

-- | Terminal marker for frontend page routes in Servant types.
-- Unlike @Get '[JSON] a@, this doesn't imply a server response —
-- it marks a URL pattern for frontend routing.
--
-- @
-- type FrontendPages =
--        "main"  :> Page
--   :<|> "login" :> Page
--   :<|> "user"  :> Capture "uid" Int :> Page
-- @
data Page

-- ─── HasRoute class ────────────────────────────────────────────

-- | Bidirectional route encoding using Servant types.
--
-- This replaces obelisk-route's @Encoder@ + @checkEncoder@ pattern.
-- Route validity is checked at compile time by Servant's type system
-- instead of at startup by the @check@ monad.
--
-- @api@ is the Servant API type, @r@ is the route sum type.
class HasRoute api r where
  -- | Encode a route value to a URL path.
  encodeRoute :: r -> Text

  -- | Decode a URL path to a route value. Returns Nothing for
  -- unrecognized paths.
  decodeRoute :: [Text] -> Map Text (Maybe Text) -> Maybe r

-- ─── URL rendering ─────────────────────────────────────────────

-- | Render a route to a URL text. Uses 'encodeRoute'.
renderRoute :: forall api r. HasRoute api r => Proxy api -> r -> Text
renderRoute _ = encodeRoute @api

-- | Render a frontend route. For compatibility with obelisk-route
-- code that uses @renderFrontendRoute enc route@.
renderFrontendRoute :: forall api r. HasRoute api r => Proxy api -> r -> Text
renderFrontendRoute = renderRoute @api

-- | Render a backend route.
renderBackendRoute :: forall api r. HasRoute api r => Proxy api -> r -> Text
renderBackendRoute = renderRoute @api

-- ─── URL parsing helpers ───────────────────────────────────────

-- | Split a URL into path segments and query parameters.
urlToSegments :: Text -> ([Text], Map Text (Maybe Text))
urlToSegments url =
  let (path, queryPart) = T.breakOn "?" url
      segments = filter (not . T.null) $ T.splitOn "/" path
      queryParams = case T.uncons queryPart of
        Just ('?', qs) -> parseQueryString qs
        _ -> Map.empty
  in (segments, queryParams)

-- | Parse a query string into key-value pairs.
parseQueryString :: Text -> Map Text (Maybe Text)
parseQueryString qs = Map.fromList
  [ case T.breakOn "=" part of
      (k, v) | T.null v  -> (k, Nothing)
              | otherwise -> (k, Just (T.drop 1 v))
  | part <- T.splitOn "&" qs
  , not (T.null part)
  ]

-- | Parse path segments into a route using 'decodeRoute'.
parseUrlSegments :: forall api r. HasRoute api r => Proxy api -> Text -> Maybe r
parseUrlSegments _ url =
  let (segs, qparams) = urlToSegments url
  in decodeRoute @api segs qparams
