{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Skeleton Common.Route rewritten with Servant types.
--
-- This replaces the 118-line Common.Route that uses obelisk-route's
-- Encoder chain, GADT routes, TH derivation, signedEncoder, etc.
module SkeletonRoute where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics (Generic)
import           Servant.API

import           Jenga.Route

-- ═══════════════════════════════════════════════════════════════
-- BEFORE (obelisk-route): 118 lines, GADTs, TH, Encoder chain
-- AFTER  (servant):       ~60 lines, simple ADTs, pattern match
-- ═══════════════════════════════════════════════════════════════

-- ─── Servant API types ─────────────────────────────────────────

-- | Frontend pages served by the SPA.
type FrontendPages =
       "app"                  :> Page           -- /app
  :<|> "login"                :> Page           -- /login
  :<|> "signup"               :> Page           -- /signup
  :<|> "reset-password"       :> Page           -- /reset-password
  :<|> "reset-password"       :> Capture "token" Text :> Page  -- /reset-password/:token
  :<|> "request-new-password" :> Page           -- /request-new-password

-- | Backend API routes.
type BackendApi =
       Page                                     -- / (landing)
  :<|> "about"      :> Page                     -- /about
  :<|> "blog"       :> Page                     -- /blog
  :<|> "robots.txt" :> Get '[PlainText] Text    -- /robots.txt
  :<|> "listen"     :> Raw                      -- /listen (rhyolite)
  :<|> "api" :> (
            "login"          :> ReqBody '[JSON] Text :> Post '[JSON] (Maybe Text)
       :<|> "reset-password" :> ReqBody '[JSON] Text :> Post '[JSON] (Maybe Text)
       :<|> "email"          :> Post '[JSON] ()
       )

-- ─── Frontend route sum type ──────────────────────────────────

data FrontendRoute
  = FR_Main
  | FR_Login
  | FR_Signup
  | FR_ResetPassword
  | FR_ResetPasswordToken Text   -- carries the signed token
  | FR_RequestNewPassword
  deriving (Eq, Show, Generic)

-- ─── HasRoute instance ─────────────────────────────────────────

instance HasRoute FrontendPages FrontendRoute where
  encodeRoute = \case
    FR_Main               -> "/app"
    FR_Login              -> "/login"
    FR_Signup             -> "/signup"
    FR_ResetPassword      -> "/reset-password"
    FR_ResetPasswordToken t -> "/reset-password/" <> t
    FR_RequestNewPassword -> "/request-new-password"

  decodeRoute segs _qparams = case segs of
    ["app"]                  -> Just FR_Main
    ["login"]                -> Just FR_Login
    ["signup"]               -> Just FR_Signup
    ["reset-password"]       -> Just FR_ResetPassword
    ["reset-password", tok]  -> Just (FR_ResetPasswordToken tok)
    ["request-new-password"] -> Just FR_RequestNewPassword
    []                       -> Just FR_Main  -- root → main
    _                        -> Nothing

-- ─── Frontend widget ───────────────────────────────────────────
--
-- @
-- import Jenga.Route.Frontend
-- import Reflex.Effectful.Jenga
--
-- frontendBody :: (WidgetEff' es t, Routed t FrontendRoute :> es) => Eff es ()
-- frontendBody = switchRoute_ $ \\case
--   FR_Main               -> el "h1" $ text "Dashboard"
--   FR_Login              -> loginWidget
--   FR_Signup             -> signupWidget
--   FR_ResetPassword      -> resetWidget Nothing
--   FR_ResetPasswordToken t -> resetWidget (Just t)
--   FR_RequestNewPassword -> requestNewPasswordWidget
-- @
--
-- ─── Entry point ───────────────────────────────────────────────
--
-- @
-- main = mainWidgetEff $ runBrowserRouting (Proxy \@FrontendPages) FR_Main frontendBody
-- @
