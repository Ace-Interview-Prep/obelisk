{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Example: Common.Route equivalent using Servant types.
--
-- == obelisk-route version (what this replaces):
--
-- @
-- data BackendRoute :: * -> * where
--   BackendRoute_Missing :: BackendRoute ()
--   BackendRoute_Api     :: BackendRoute PageName
--
-- data FrontendRoute :: * -> * where
--   FrontendRoute_Main          :: FrontendRoute ()
--   FrontendRoute_Login         :: FrontendRoute ()
--   FrontendRoute_Signup        :: FrontendRoute ()
--   FrontendRoute_ResetPassword :: FrontendRoute ()
--
-- fullRouteEncoder = mkFullRouteEncoder
--   (FullRoute_Frontend (JengaRoute_App FrontendRoute_Main) :/ ())
--   (\\case
--     BackendRoute_Missing -> PathSegment "missing" (unitEncoder mempty)
--     BackendRoute_Api     -> PathSegment "api" pathOnlyEncoder)
--   (\\case
--     FrontendRoute_Main          -> PathEnd (unitEncoder mempty)
--     FrontendRoute_Login         -> PathSegment "login" (unitEncoder mempty)
--     FrontendRoute_Signup        -> PathSegment "signup" (unitEncoder mempty)
--     FrontendRoute_ResetPassword -> PathSegment "reset-password" (unitEncoder mempty))
-- @
module CommonRoute where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as T
import           Servant.API

import           Jenga.Route

-- ─── Servant API type ──────────────────────────────────────────
-- This IS the route definition. No GADT, no Encoder chain.
-- Servant's type-level combinators express the same structure
-- that pathComponentEncoder + SegmentResult expressed at the value level.

-- | Frontend page routes.
type FrontendPages =
       Page                                                 -- /        → Main
  :<|> "login"          :> Page                             -- /login
  :<|> "signup"         :> Page                             -- /signup
  :<|> "reset-password" :> Page                             -- /reset-password
  :<|> "dashboard"      :> Capture "uid" Int :> Page        -- /dashboard/:uid
  :<|> "settings"       :> Page                             -- /settings

-- | Backend API routes.
type BackendApi =
  "api" :> (
       "ping"  :> Get '[JSON] Text
  :<|> "users" :> Capture "id" Int :> Get '[JSON] Text
  )

-- | Full app = frontend pages + backend API.
type App = FrontendPages :<|> BackendApi

-- ─── Route sum type ────────────────────────────────────────────
-- Simple ADT. No GADT, no type parameter, no DSum.

data FrontendRoute
  = FR_Main
  | FR_Login
  | FR_Signup
  | FR_ResetPassword
  | FR_Dashboard Int
  | FR_Settings
  deriving (Eq, Show)

-- ─── HasRoute instance ─────────────────────────────────────────
-- Bidirectional encoding. Replaces the entire Encoder chain.

instance HasRoute FrontendPages FrontendRoute where
  encodeRoute = \case
    FR_Main          -> "/"
    FR_Login         -> "/login"
    FR_Signup        -> "/signup"
    FR_ResetPassword -> "/reset-password"
    FR_Dashboard uid -> "/dashboard/" <> T.pack (show uid)
    FR_Settings      -> "/settings"

  decodeRoute segments _queryParams = case segments of
    []                 -> Just FR_Main
    ["login"]          -> Just FR_Login
    ["signup"]         -> Just FR_Signup
    ["reset-password"] -> Just FR_ResetPassword
    ["dashboard", uid] -> FR_Dashboard <$> readMaybe (T.unpack uid)
    ["settings"]       -> Just FR_Settings
    _                  -> Nothing

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
  [(a, "")] -> Just a
  _         -> Nothing

-- ─── What users write in Frontend.hs ───────────────────────────
--
-- @
-- import Reflex.Effectful.Jenga
-- import Jenga.Route.Frontend
-- import CommonRoute
--
-- frontendBody :: (WidgetEff' es t, Routed t FrontendRoute :> es) => Eff es ()
-- frontendBody = switchRoute_ $ \\case
--   FR_Main          -> el "h1" $ text "Home"
--   FR_Login         -> loginWidget
--   FR_Signup        -> signupWidget
--   FR_ResetPassword -> resetWidget
--   FR_Dashboard uid -> dashboardWidget uid
--   FR_Settings      -> settingsWidget
--
-- main = mainWidgetEff $ runBrowserRouting (Proxy \@FrontendPages) FR_Main frontendBody
-- @
