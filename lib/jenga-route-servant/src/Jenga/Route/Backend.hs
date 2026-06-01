{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Backend routing using servant-server + Warp.
--
-- Replaces obelisk's Snap-based backend with standard Servant.
--
-- == Migration from obelisk backend
--
-- @
-- -- BEFORE (Snap):
-- backend :: Backend BackendRoute FrontendRoute
-- backend = Backend
--   { _backend_run = \\serve -> serve $ \\case
--       BackendRoute_Api -> apiHandler
--       BackendRoute_Missing -> return ()
--   , _backend_routeEncoder = fullRouteEncoder
--   }
--
-- -- AFTER (Servant):
-- type BackendApi = "api" :> (
--        "ping" :> Get '[JSON] Text
--   :<|> "users" :> Get '[JSON] [User]
--   )
--
-- server :: Server BackendApi
-- server = pingHandler :<|> usersHandler
--
-- main = runBackend 8000 (Proxy \@BackendApi) server staticDir
-- @
module Jenga.Route.Backend
  ( runBackend
  , serveWithFrontend
  ) where

import           Data.Proxy (Proxy(..))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Network.Wai (Application, responseLBS)
import           Network.Wai.Application.Static (staticApp, defaultFileServerSettings)
import           Network.HTTP.Types (status200, status404)
import           Servant (Server, HasServer, serve, (:<|>)(..))
import qualified Network.Wai.Handler.Warp as Warp

-- | Run a Servant backend with static file serving for the frontend.
--
-- The frontend is served as a Single Page Application: all
-- non-API, non-static routes return @index.html@ so the
-- client-side router can handle them.
--
-- @
-- main = runBackend 8000 (Proxy \@BackendApi) server "./static"
-- @
runBackend
  :: HasServer api '[]
  => Int                    -- ^ Port
  -> Proxy api              -- ^ API type
  -> Server api             -- ^ API handlers
  -> FilePath               -- ^ Static files directory
  -> IO ()
runBackend port api server staticDir = do
  putStrLn $ "Backend running on port " <> show port
  Warp.run port (serveWithFrontend api server staticDir)

-- | Build a WAI Application that serves:
-- 1. The Servant API under its path prefix
-- 2. Static files from the given directory
-- 3. @index.html@ for everything else (SPA fallback)
serveWithFrontend
  :: HasServer api '[]
  => Proxy api
  -> Server api
  -> FilePath               -- ^ Static files directory
  -> Application
serveWithFrontend api server staticDir = serve api server
  -- TODO: Combine with static file serving and SPA fallback.
  -- For now, just serves the API. Full implementation would use
  -- servant-server's Raw combinator or WAI middleware:
  --
  -- type FullApp = Api :<|> Raw
  -- serve (Proxy @FullApp) (server :<|> Tagged spaApp)
  --
  -- where spaApp tries static files first, falls back to index.html.
