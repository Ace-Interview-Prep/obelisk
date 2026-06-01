{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Common.Route where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T

import Jenga.Route

#if !defined(javascript_HOST_ARCH) && !defined(wasm32_HOST_ARCH)
import Data.Functor.Identity (Identity)
import Database.Beam.Schema (PrimaryKey)
import Rhyolite.Account (Account)
import Data.Signed (Signed(..))
import Servant.API

type Id a = PrimaryKey a Identity
type SignedAccountToken = Signed (Id Account)
#else
import Data.Signed (Signed(..))
data AccountId
type SignedAccountToken = Signed AccountId
#endif

-- ─── Servant API types (native only) ───────────────────────────

#if !defined(javascript_HOST_ARCH) && !defined(wasm32_HOST_ARCH)
type FrontendPages =
       "app"                  :> Page
  :<|> "login"                :> Page
  :<|> "signup"               :> Page
  :<|> "reset-password"       :> Page
  :<|> "reset-password"       :> Capture "token" Text :> Page
  :<|> "request-new-password" :> Page

type BackendApi =
       Page
  :<|> "about"      :> Page
  :<|> "blog"       :> Page
  :<|> "robots.txt" :> Get '[PlainText] Text
  :<|> "listen"     :> Raw
  :<|> "api" :> (
            "login"          :> ReqBody '[JSON] Text :> Post '[JSON] (Maybe Text)
       :<|> "reset-password" :> ReqBody '[JSON] Text :> Post '[JSON] (Maybe Text)
       :<|> "email"          :> Post '[JSON] ()
       )
#else
data FrontendPages
#endif

-- ─── Frontend route sum type ───────────────────────────────────

data FrontendRoute
  = FrontendRoute_Main
  | FrontendRoute_Login
  | FrontendRoute_Signup
  | FrontendRoute_ResetPassword
  | FrontendRoute_ResetPasswordToken Text
  | FrontendRoute_RequestNewPassword
  deriving stock (Eq, Show)

-- ─── HasRoute instance ─────────────────────────────────────────

instance HasRoute FrontendPages FrontendRoute where
  encodeRoute = \case
    FrontendRoute_Main               -> "/app"
    FrontendRoute_Login              -> "/login"
    FrontendRoute_Signup             -> "/signup"
    FrontendRoute_ResetPassword      -> "/reset-password"
    FrontendRoute_ResetPasswordToken t -> "/reset-password/" <> t
    FrontendRoute_RequestNewPassword -> "/request-new-password"

  decodeRoute segs _qparams = case segs of
    ["app"]                  -> Just FrontendRoute_Main
    ["login"]                -> Just FrontendRoute_Login
    ["signup"]               -> Just FrontendRoute_Signup
    ["reset-password"]       -> Just FrontendRoute_ResetPassword
    ["reset-password", tok]  -> Just (FrontendRoute_ResetPasswordToken tok)
    ["request-new-password"] -> Just FrontendRoute_RequestNewPassword
    []                       -> Just FrontendRoute_Main
    _                        -> Nothing
