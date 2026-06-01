{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reflex.Effectful.Effect.Jenga.Route
  ( -- * Routed effect
    Routed(..)
  , askRoute

    -- * SetRoute effect
  , SetRoute(..)
  , setRoute
  , modifyRoute

    -- * RouteToUrl effect
  , RouteToUrl(..)
  , askRouteToUrl
  ) where

import           Data.Text                (Text)

import           Effectful                (Effect, Dispatch(..), DispatchOf, (:>), Eff)
import           Effectful.Dispatch.Dynamic (send)
import           Reflex                   (Dynamic, Event)

-- ─── Routed ────────────────────────────────────────────────────

data Routed t r :: Effect where
  AskRoute :: Routed t r m (Dynamic t r)

type instance DispatchOf (Routed t r) = 'Dynamic

askRoute :: Routed t r :> es => Eff es (Dynamic t r)
askRoute = send AskRoute

-- ─── SetRoute ──────────────────────────────────────────────────

data SetRoute t r :: Effect where
  SetRoute    :: Event t r -> SetRoute t r m ()
  ModifyRoute :: Event t (r -> r) -> SetRoute t r m ()

type instance DispatchOf (SetRoute t r) = 'Dynamic

setRoute :: SetRoute t r :> es => Event t r -> Eff es ()
setRoute = send . SetRoute

modifyRoute :: SetRoute t r :> es => Event t (r -> r) -> Eff es ()
modifyRoute = send . ModifyRoute

-- ─── RouteToUrl ────────────────────────────────────────────────

data RouteToUrl r :: Effect where
  AskRouteToUrl :: RouteToUrl r m (r -> Text)

type instance DispatchOf (RouteToUrl r) = 'Dynamic

askRouteToUrl :: RouteToUrl r :> es => Eff es (r -> Text)
askRouteToUrl = send AskRouteToUrl
