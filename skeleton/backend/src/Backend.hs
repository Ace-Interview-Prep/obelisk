{-# LANGUAGE OverloadedStrings #-}
module Backend where

import Snap.Core (pass)

import Jenga.Backend
import Common.Route (FrontendPages, FrontendRoute(..))
import Jenga.Frontend (Frontend(..))

import Landing

backend :: Backend FrontendPages FrontendRoute
backend = Backend
  { _backend_apiHandler = do
      -- TODO: API routes via servant-snap or Snap handlers
      pass
  , _backend_frontend = frontendForSSR
  , _backend_fallbackRoute = FrontendRoute_Main
  }

-- Minimal frontend stub for SSR. The real frontend runs in the browser.
frontendForSSR :: Frontend FrontendPages FrontendRoute
frontendForSSR = Frontend
  { _frontend_head = el "title" $ text "Jenga App"
  , _frontend_body = text "Loading..."
  }
