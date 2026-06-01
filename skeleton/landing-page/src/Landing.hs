{-# LANGUAGE OverloadedStrings #-}
module Landing
  ( serveLandingRoute
  , serveRobotsTxt
  ) where

import Landing.Pages

import Data.Text (Text)
import Snap.Core
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text.Encoding as TE

-- | Serve landing pages. Called from the backend's route handler.
serveLandingRoute :: MonadSnap m => m ()
serveLandingRoute = route
  [ ("", ifTop $ serveHtml indexHtml)
  , ("about", serveHtml aboutHtml)
  , ("blog", serveHtml blogIndexHtml)
  ]

-- | Serve robots.txt
serveRobotsTxt :: MonadSnap m => m ()
serveRobotsTxt = do
  modifyResponse $ setContentType "text/plain; charset=utf-8"
  writeBS $ BSC.pack "User-agent: *\nAllow: /\n"

-- | Helper to serve HTML content.
serveHtml :: MonadSnap m => Text -> m ()
serveHtml html = do
  modifyResponse $ setContentType "text/html; charset=utf-8"
  writeBS $ TE.encodeUtf8 html
