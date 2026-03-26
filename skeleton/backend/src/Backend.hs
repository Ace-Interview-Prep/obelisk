module Backend where

import Obelisk.Backend
import Obelisk.Route

import Common
import Landing


backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> serve $ \case
      r@(BackendRoute_Landing :/ ()) -> serveLandingRoute r
      r@(BackendRoute_About :/ ()) -> serveLandingRoute r
      r@(BackendRoute_Blog :/ ()) -> serveLandingRoute r
      BackendRoute_RobotsTxt :/ () -> serveRobotsTxt
      
      BackendRoute_Listen :/ () -> pure () -- TODO: rhyolite listen handler
      BackendRoute_Api :/ apiRoute -> case apiRoute of
        ApiRoute_Login :/ _msid -> pure () -- TODO: jenga-auth login handler
        ApiRoute_ResetPassword :/ _msid -> pure () -- TODO: jenga-auth reset password handler
        ApiRoute_Email :/ () -> pure () -- TODO: jenga-auth email handler
  , _backend_routeEncoder = fullRouteEncoder
  }
