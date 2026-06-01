{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import Data.Default (def)
import Data.Text (Text, pack)
import Reflex (leftmost, updated, ffor)
import Reflex.Dom.Builder.Class (domEvent, EventName(Click))
import Reflex.Dom.Core (_inputElement_value)

import Jenga.Frontend (Frontend(..))
import Jenga.Route.Frontend
import Reflex.Effectful
import Reflex.Effectful.Run (WidgetEff')
import Reflex.Effectful.Types (T)

import Common.Route

frontend :: Frontend FrontendPages FrontendRoute
frontend = Frontend
  { _frontend_head = frontendHead
  , _frontend_body = frontendBody
  }

frontendHead :: (WidgetEff' es t, Routed t FrontendRoute :> es) => Eff es ()
frontendHead = do
  el "title" $ text "Jenga Effectful"

frontendBody :: (WidgetEff' es t, Routed t FrontendRoute :> es, SetRoute t FrontendRoute :> es, RouteToUrl FrontendRoute :> es) => Eff es ()
frontendBody = do
  -- Navigation bar
  el "nav" $ do
    navBtn FrontendRoute_Main "Home"
    text " | "
    navBtn FrontendRoute_Login "Login"
    text " | "
    navBtn FrontendRoute_Signup "Signup"
    text " | "
    navBtn FrontendRoute_ResetPassword "Reset"

  el "hr" blank

  -- Route-based page switching
  switchRoute_ $ \case
    FrontendRoute_Main -> mainPage
    FrontendRoute_Login -> loginPage
    FrontendRoute_Signup -> signupPage
    FrontendRoute_ResetPassword -> resetPage
    FrontendRoute_ResetPasswordToken tok -> resetPage
    FrontendRoute_RequestNewPassword -> resetPage

-- Navigation button that sets the route on click
navBtn :: (WidgetEff' es t, SetRoute t FrontendRoute :> es) => FrontendRoute -> Text -> Eff es ()
navBtn route label = do
  (e, _) <- el' "button" $ text label
  setRoute (route <$ domEvent Click e)

-- ─── Pages ─────────────────────────────────────────────────────

mainPage :: WidgetEff' es t => Eff es ()
mainPage = do
  el "h1" $ text "Jenga + Effectful"
  el "p" $ text "GHC 9.14 | reflex-effectful | Servant routing"
  el "hr" blank
  el "h2" $ text "Counter"
  counterWidget
  el "hr" blank
  el "h2" $ text "Input Echo"
  inputWidget

counterWidget :: WidgetEff' es t => Eff es ()
counterWidget = do
  el "div" $ do
    (incEl, _) <- el' "button" $ text "+"
    (decEl, _) <- el' "button" $ text "-"
    val <- foldDyn (+) (0 :: Int) (leftmost [1 <$ domEvent Click incEl, (-1) <$ domEvent Click decEl])
    text " Value: "
    display val

inputWidget :: forall es t. WidgetEff' es t => Eff es ()
inputWidget = do
  inp <- inputElement @(T es) def
  el "p" $ do
    text "You typed: "
    dynText (_inputElement_value inp)

loginPage :: forall es t. WidgetEff' es t => Eff es ()
loginPage = do
  el "h1" $ text "Login"
  el "div" $ do
    el "p" $ text "Email:"
    _ <- inputElement @(T es) def
    el "p" $ text "Password:"
    _ <- inputElement @(T es) def
    el "br" blank
    (btn, _) <- el' "button" $ text "Sign In"
    ct <- count (domEvent Click btn)
    el "p" $ do
      text "Attempts: "
      display ct

signupPage :: forall es t. WidgetEff' es t => Eff es ()
signupPage = do
  el "h1" $ text "Sign Up"
  el "p" $ text "Create your account to get started."
  _ <- inputElement @(T es) def
  el "br" blank
  (btn, _) <- el' "button" $ text "Create Account"
  clicked <- holdDyn False (True <$ domEvent Click btn)
  dyn_ $ ffor clicked $ \b ->
    if b then el "p" $ text "Account created!"
         else blank

resetPage :: forall es t. WidgetEff' es t => Eff es ()
resetPage = do
  el "h1" $ text "Reset Password"
  el "p" $ text "Enter your email to reset your password."
  _ <- inputElement @(T es) def
  (btn, _) <- el' "button" $ text "Send Reset Link"
  sent <- holdDyn False (True <$ domEvent Click btn)
  dyn_ $ ffor sent $ \b ->
    if b then el "p" $ text "Reset link sent! Check your email."
         else blank
