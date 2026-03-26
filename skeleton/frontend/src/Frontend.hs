module Frontend where

import Obelisk.Frontend
import Obelisk.Generated.Static ()
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom

import Common.Route



frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = frontendHead
  , _frontend_body = frontendBody
  }
 
frontendHead :: ObeliskWidget t route m => RoutedT t route m ()
frontendHead = do
  el "title" $ text "Obelisk App"

frontendBody :: ObeliskWidget t (R FrontendRoute) m => RoutedT t (R FrontendRoute) m ()
frontendBody = subRoute_ $ \case
  FrontendRoute_Main -> do
    el "h1" $ text "Obelisk App"
    el "p" $ text "Edit frontend/src/Frontend.hs to get started."
  FrontendRoute_Login -> do
    el "h1" $ text "Login"
    el "p" $ text "TODO: Login form"
  FrontendRoute_Signup -> do
    el "h1" $ text "Sign Up"
    el "p" $ text "TODO: Signup form"
  FrontendRoute_ResetPassword -> do
    el "h1" $ text "Reset Password"
    el "p" $ text "TODO: Reset password form"
  FrontendRoute_RequestNewPassword -> do
    el "h1" $ text "Request New Password"
    el "p" $ text "TODO: Request password reset form"
