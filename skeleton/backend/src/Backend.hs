{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Backend where

import Data.Aeson (Value, object, (.=))
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Snap.Core (Snap)

import Servant.API ((:<|>)(..))
import Servant.Server (Server)

import Jenga.Backend.Servant
import Common.Route (FrontendPages, FrontendRoute(..), SkeletonApi)
import Jenga.Frontend (Frontend(..))
import Reflex.Effectful (el, text)

import Landing ()

-- ─── Servant handlers ────────────────────────────────────────

skeletonServer :: Server SkeletonApi '[] Snap
skeletonServer = helloHandler :<|> echoHandler

helloHandler :: Snap Value
helloHandler = pure $ object
  [ "message" .= ("Hello from Jenga backend!" :: Text)
  , "users"   .= (42 :: Int)
  , "stack"   .= ("Effectful+ClasshSS+GridEff+Servant" :: Text)
  ]

echoHandler :: Value -> Snap Value
echoHandler = pure

-- ─── Backend ─────────────────────────────────────────────────

backend :: Backend FrontendPages FrontendRoute
backend = Backend
  { _backend_apiHandler = serveSnap (Proxy @SkeletonApi) skeletonServer
  , _backend_frontend = frontendForSSR
  , _backend_fallbackRoute = FrontendRoute_Main
  }

-- Minimal frontend stub for SSR. The real frontend runs in the browser.
frontendForSSR :: Frontend FrontendPages FrontendRoute
frontendForSSR = Frontend
  { _frontend_head = el "title" $ text "Jenga App"
  , _frontend_body = text "Loading..."
  }
