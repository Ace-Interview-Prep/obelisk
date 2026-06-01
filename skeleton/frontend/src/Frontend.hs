{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Frontend where

import Jenga.Generated.Static ()
import Jenga.Route.Frontend
import Reflex.Effectful
import Reflex.Effectful.Run (WidgetEff')

import Common.Route

frontend :: Frontend FrontendPages FrontendRoute
frontend = Frontend
  { _frontend_head = frontendHead
  , _frontend_body = frontendBody
  }

frontendHead :: (WidgetEff' es t, Routed t FrontendRoute :> es) => Eff es ()
frontendHead = do
  el "title" $ text "Jenga App"

frontendBody :: (WidgetEff' es t, Routed t FrontendRoute :> es, SetRoute t FrontendRoute :> es, RouteToUrl FrontendRoute :> es) => Eff es ()
frontendBody = switchRoute_ $ \case
  FrontendRoute_Main -> do
    el "h1" $ text "Jenga App"
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
  FrontendRoute_ResetPasswordToken _tok -> do
    el "h1" $ text "Reset Password"
    el "p" $ text "TODO: Reset password with token"
  FrontendRoute_RequestNewPassword -> do
    el "h1" $ text "Request New Password"
    el "p" $ text "TODO: Request password reset form"
