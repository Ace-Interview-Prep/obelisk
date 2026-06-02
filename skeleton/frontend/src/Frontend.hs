{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Frontend where

import Control.Concurrent (threadDelay)
import Data.Text (Text, pack)
import qualified Data.Map as Map
import Reflex (leftmost, ffor, updated)
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
frontendHead = do
  el "title" $ text "Jenga Effectful"
  -- Inline styles for a nicer look
  el "style" $ text css

frontendBody :: (WidgetEff' es t, Routed t FrontendRoute :> es, SetRoute t FrontendRoute :> es, RouteToUrl FrontendRoute :> es) => Eff es ()
frontendBody = do
  elClass "div" "app" $ do
    navbar
    elClass "div" "content" $ do
      switchRoute_ $ \case
        FrontendRoute_Main -> homePage
        FrontendRoute_Login -> loginPage
        FrontendRoute_Signup -> signupPage
        FrontendRoute_ResetPassword -> resetPage
        _ -> resetPage

-- ─── Navigation ────────────────────────────────────────────────

navbar :: (WidgetEff' es t, SetRoute t FrontendRoute :> es, Routed t FrontendRoute :> es) => Eff es ()
navbar = elClass "nav" "navbar" $ do
  elClass "span" "logo" $ text "Jenga"
  elClass "div" "nav-links" $ do
    navLink FrontendRoute_Main "Home"
    navLink FrontendRoute_Login "Login"
    navLink FrontendRoute_Signup "Sign Up"

navLink :: (WidgetEff' es t, SetRoute t FrontendRoute :> es) => FrontendRoute -> Text -> Eff es ()
navLink route label = do
  (e, _) <- elAttr "a" (Map.fromList [("href", "#"), ("class", "nav-link")]) $ text label
  setRoute (route <$ domEvent Click e)

-- ─── Home Page ─────────────────────────────────────────────────

homePage :: WidgetEff' es t => Eff es ()
homePage = do
  elClass "div" "hero" $ do
    el "h1" $ text "Jenga + Effectful"
    elClass "p" "subtitle" $ text "Full-stack Haskell with GHC 9.14 | Effectful effects | Servant routing"

  elClass "div" "cards" $ do
    -- Counter card
    elClass "div" "card" $ do
      el "h2" $ text "Counter"
      el "div" $ do
        (dec, _) <- elAttr "button" (Map.singleton "class" "btn") $ text "-"
        (inc, _) <- elAttr "button" (Map.singleton "class" "btn") $ text "+"
        val <- foldDyn (+) (0 :: Int) (leftmost [1 <$ domEvent Click inc, (-1) <$ domEvent Click dec])
        elClass "span" "counter-val" $ display val

    -- API call card
    elClass "div" "card" $ do
      el "h2" $ text "API Response"
      (fetchBtn, _) <- elAttr "button" (Map.singleton "class" "btn") $ text "Fetch Data"
      let fetchEv = domEvent Click fetchBtn

      -- Simulate API call with performEvent
      responseEv <- performEvent $ ffor fetchEv $ \_ -> do
        threadDelay 500000  -- simulate network delay
        pure ("{ \"status\": \"ok\", \"users\": 42, \"server\": \"GHC 9.14.1\" }" :: Text)

      response <- holdDyn "Click 'Fetch Data' to call the API" responseEv
      elClass "pre" "api-response" $ dynText response

    -- Dynamic widget card
    elClass "div" "card" $ do
      el "h2" $ text "Dynamic Toggle"
      (toggleBtn, _) <- elAttr "button" (Map.singleton "class" "btn") $ text "Toggle"
      showDetail <- toggle False (domEvent Click toggleBtn)
      dyn_ $ ffor showDetail $ \b ->
        if b
          then elClass "div" "detail" $ do
            el "p" $ text "This widget was dynamically swapped in!"
            el "p" $ text "widgetHold powers Adjustable — widgets replace each other on the fly."
          else elClass "div" "detail" $ do
            el "p" $ text "Click Toggle to swap this widget."

-- ─── Login Page ────────────────────────────────────────────────

loginPage :: WidgetEff' es t => Eff es ()
loginPage = elClass "div" "form-page" $ do
  el "h1" $ text "Login"
  elClass "p" "form-hint" $ text "Demo: click Sign In to simulate authentication"
  (btn, _) <- elAttr "button" (Map.singleton "class" "btn primary") $ text "Sign In"

  -- Simulate auth flow
  authEv <- performEvent $ ffor (domEvent Click btn) $ \_ -> do
    threadDelay 800000  -- simulate API call
    pure ("Authenticated as admin@jenga.dev" :: Text)

  status <- holdDyn "" authEv
  elClass "p" "status" $ dynText status

-- ─── Signup Page ───────────────────────────────────────────────

signupPage :: WidgetEff' es t => Eff es ()
signupPage = elClass "div" "form-page" $ do
  el "h1" $ text "Create Account"
  elClass "p" "form-hint" $ text "Demo: click to create an account"
  (btn, _) <- elAttr "button" (Map.singleton "class" "btn primary") $ text "Create Account"

  signupEv <- performEvent $ ffor (domEvent Click btn) $ \_ -> do
    threadDelay 600000
    pure ("Account created! Welcome to Jenga." :: Text)

  msg <- holdDyn "" signupEv
  elClass "p" "status success" $ dynText msg

-- ─── Reset Page ────────────────────────────────────────────────

resetPage :: WidgetEff' es t => Eff es ()
resetPage = elClass "div" "form-page" $ do
  el "h1" $ text "Reset Password"
  (btn, _) <- elAttr "button" (Map.singleton "class" "btn") $ text "Send Reset Link"
  sent <- holdDyn False (True <$ domEvent Click btn)
  dyn_ $ ffor sent $ \b ->
    if b then elClass "p" "status success" $ text "Reset link sent! Check your email."
         else elClass "p" "form-hint" $ text "Enter your email to reset your password."

-- ─── Styles ────────────────────────────────────────────────────

css :: Text
css = "\
  \* { margin: 0; padding: 0; box-sizing: border-box; }\
  \body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; background: #0f172a; color: #e2e8f0; }\
  \.app { max-width: 900px; margin: 0 auto; padding: 20px; }\
  \.navbar { display: flex; align-items: center; padding: 16px 0; border-bottom: 1px solid #1e293b; margin-bottom: 32px; }\
  \.logo { font-size: 24px; font-weight: bold; color: #38bdf8; margin-right: 32px; }\
  \.nav-links { display: flex; gap: 16px; }\
  \.nav-link { color: #94a3b8; text-decoration: none; cursor: pointer; padding: 8px 16px; border-radius: 8px; transition: all 0.2s; }\
  \.nav-link:hover { color: #e2e8f0; background: #1e293b; }\
  \.hero { text-align: center; padding: 48px 0; }\
  \.hero h1 { font-size: 36px; color: #f1f5f9; margin-bottom: 12px; }\
  \.subtitle { color: #64748b; font-size: 16px; }\
  \.cards { display: grid; grid-template-columns: repeat(auto-fit, minmax(260px, 1fr)); gap: 20px; margin-top: 32px; }\
  \.card { background: #1e293b; border-radius: 12px; padding: 24px; border: 1px solid #334155; }\
  \.card h2 { font-size: 18px; color: #38bdf8; margin-bottom: 16px; }\
  \.btn { background: #334155; color: #e2e8f0; border: none; padding: 8px 16px; border-radius: 6px; cursor: pointer; font-size: 14px; margin: 4px; }\
  \.btn:hover { background: #475569; }\
  \.btn.primary { background: #2563eb; }\
  \.btn.primary:hover { background: #1d4ed8; }\
  \.counter-val { font-size: 32px; font-weight: bold; color: #38bdf8; margin: 0 16px; }\
  \.api-response { background: #0f172a; padding: 12px; border-radius: 6px; font-size: 13px; color: #4ade80; overflow-x: auto; margin-top: 12px; white-space: pre-wrap; }\
  \.detail { padding: 12px; background: #0f172a; border-radius: 6px; margin-top: 12px; }\
  \.detail p { margin: 4px 0; color: #94a3b8; }\
  \.form-page { max-width: 400px; margin: 0 auto; text-align: center; padding-top: 48px; }\
  \.form-page h1 { margin-bottom: 16px; }\
  \.form-hint { color: #64748b; margin-bottom: 24px; }\
  \.status { margin-top: 16px; color: #38bdf8; }\
  \.status.success { color: #4ade80; }\
  \hr { border-color: #1e293b; margin: 24px 0; }\
  \"
