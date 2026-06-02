{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Frontend where

import Reflex (leftmost, ffor)
import Data.Text (Text)
import Reflex.Dom.Builder.Class (domEvent, EventName(Click))

import Jenga.Frontend (Frontend(..))
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
frontendHead = el "title" $ text "Jenga Effectful"

frontendBody :: (WidgetEff' es t, Routed t FrontendRoute :> es, SetRoute t FrontendRoute :> es, RouteToUrl FrontendRoute :> es) => Eff es ()
frontendBody = do
  el "nav" $ do
    navBtn FrontendRoute_Main "Home"
    text " | "
    navBtn FrontendRoute_Login "Login"
    text " | "
    navBtn FrontendRoute_Signup "Signup"
  el "hr" blank
  switchRoute_ $ \case
    FrontendRoute_Main -> mainPage
    FrontendRoute_Login -> loginPage
    FrontendRoute_Signup -> signupPage
    FrontendRoute_ResetPassword -> el "h1" $ text "Reset"
    FrontendRoute_ResetPasswordToken _ -> el "h1" $ text "Reset"
    FrontendRoute_RequestNewPassword -> el "h1" $ text "Request Reset"

navBtn :: (WidgetEff' es t, SetRoute t FrontendRoute :> es) => FrontendRoute -> Text -> Eff es ()
navBtn route label = do
  (e, _) <- el' "button" $ text label
  setRoute (route <$ domEvent Click e)

mainPage :: WidgetEff' es t => Eff es ()
mainPage = do
  el "h1" $ text "Jenga + Effectful"
  el "p" $ text "GHC 9.14 | reflex-effectful | Servant routing"
  el "hr" blank
  el "h2" $ text "Counter"
  el "div" $ do
    (incEl, _) <- el' "button" $ text "+"
    (decEl, _) <- el' "button" $ text "-"
    val <- foldDyn (+) (0 :: Int) (leftmost [1 <$ domEvent Click incEl, (-1) <$ domEvent Click decEl])
    text " Value: "
    display val

loginPage :: WidgetEff' es t => Eff es ()
loginPage = do
  el "h1" $ text "Login"
  (btn, _) <- el' "button" $ text "Sign In"
  ct <- count (domEvent Click btn)
  el "p" $ do
    text "Clicks: "
    display ct

signupPage :: WidgetEff' es t => Eff es ()
signupPage = do
  el "h1" $ text "Sign Up"
  (btn, _) <- el' "button" $ text "Create Account"
  done <- holdDyn False (True <$ domEvent Click btn)
  dyn_ $ ffor done $ \b ->
    if b then el "p" $ text "Account created!"
         else el "p" $ text "Click to create."
