module Landing
  ( serveLandingRoute
  , serveRobotsTxt
  ) where

import Common.Route

import Landing.Pages

import Obelisk.Route

import Data.Text (Text)
import Snap.Core
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text.Encoding as TE



-- | Dispatch all landing-related backend routes.
--
-- For production sites, replace the inline HTML in Landing.Pages with
-- lamarckian TH-compiled pages:
--
-- @
-- import Lamarckian.Compiler (compileStaticSite)
-- import Lamarckian.Snap (serveCompressed)
-- import Landing.Impl (siteCfg)
--
-- serveLandingRoute route = case route of
--   BackendRoute_Landing :/ () ->
--     serveCompressed $(compileStaticSite siteCfg landingRoute indexPage)
--   ...
-- @
serveLandingRoute :: MonadSnap m => R BackendRoute -> m ()
serveLandingRoute = \case
  BackendRoute_Landing :/ () -> serveHtml indexHtml
  BackendRoute_About :/ () -> serveHtml aboutHtml
  BackendRoute_Blog :/ () -> serveHtml blogIndexHtml
  _ -> pass

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
