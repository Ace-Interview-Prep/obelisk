module Reflex.Effectful.Jenga
  ( -- * Jenga-specific effects
    module Reflex.Effectful.Effect.Jenga.Configs
  , module Reflex.Effectful.Effect.Jenga.Cookies
  , module Reflex.Effectful.Effect.Jenga.Route

    -- * Combined constraint aliases
  , JengaWidget'

    -- * Re-export core
  , module Reflex.Effectful
  ) where

import           Effectful                (IOE, (:>))

import           Reflex.Effectful
import           Reflex.Effectful.Effect.Jenga.Configs
import           Reflex.Effectful.Effect.Jenga.Cookies
import           Reflex.Effectful.Effect.Jenga.Route

-- | Full Jenga widget constraint — everything a frontend widget needs.
--
-- Replaces the 20+ MTL constraint @JengaWidget t route m@:
--
-- @
-- -- BEFORE (MTL, 20+ constraints):
-- myPage :: JengaWidget t route m => RoutedT t route m ()
-- myPage = subRoute_ $ \\case
--   FrontendRoute_Main -> el "h1" $ text "Home"
--
-- -- AFTER (Effectful, one constraint):
-- myPage :: JengaWidget' es t (R FrontendRoute) => Eff es ()
-- myPage = subRoute_ $ \\case
--   FrontendRoute_Main -> el "h1" $ text "Home"
-- @
type JengaWidget' es t r =
  ( WidgetEff' es t
  , Configs         :> es
  , CookiesEff      :> es
  , Routed t r      :> es
  , SetRoute t r    :> es
  , RouteToUrl r    :> es
  , IOE             :> es
  )
