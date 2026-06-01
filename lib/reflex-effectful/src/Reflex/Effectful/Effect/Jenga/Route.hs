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

    -- * Routing combinators
  , subRoute_
  , subRoute
  ) where

import           Data.Dependent.Sum       (DSum(..))
import           Data.Functor.Identity    (Identity(..))
import           Data.Text                (Text)

import           Effectful                (Effect, Dispatch(..), DispatchOf, (:>), Eff)
import           Effectful.Dispatch.Dynamic (send)
import           Reflex                   (Dynamic, Event, Reflex)
import qualified Reflex
import           Data.GADT.Compare        (GEq)

import           Reflex.Effectful.Types   (KnownTimeline)
import           Reflex.Effectful.Effect.Hold (Hold, holdDyn)
import           Reflex.Effectful.Effect.Sample (Sample, sample)
import           Reflex.Effectful.Effect.Adjustable (Adjustable, runWithReplace)

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

-- ─── Routing combinators ───────────────────────────────────────

-- | Route on a sum type. Replaces @subRoute_@ from obelisk.
--
-- @
-- frontendBody :: JengaWidget' es t (R FrontendRoute) => Eff es ()
-- frontendBody = subRoute_ $ \\case
--   FrontendRoute_Main -> el "h1" $ text "Home"
--   FrontendRoute_Login -> el "h1" $ text "Login"
-- @
subRoute_
  :: forall t r es.
     ( KnownTimeline es t, Routed t (DSum r Identity) :> es
     , Adjustable t :> es, Hold t :> es, Sample t :> es
     , GEq r, Reflex t
     )
  => (forall a. r a -> Eff es ())
  -> Eff es ()
subRoute_ f = do
  routeDyn <- askRoute
  -- Factor the DSum dynamic: split tag from value
  factoredDyn <- factorDynEff routeDyn
  -- Get initial value and run initial widget
  initial <- sample (Reflex.current factoredDyn)
  let runForDSum (tag :=> valDyn) = f tag
  (_, _) <- runWithReplace (runForDSum initial) (runForDSum <$> Reflex.updated factoredDyn)
  pure ()

-- | Route on a sum type, returning a Dynamic of results.
subRoute
  :: forall t r es b.
     ( KnownTimeline es t, Routed t (DSum r Identity) :> es
     , Adjustable t :> es, Hold t :> es, Sample t :> es
     , GEq r, Reflex t
     )
  => (forall a. r a -> Eff es b)
  -> Eff es (Dynamic t b)
subRoute f = do
  routeDyn <- askRoute
  factoredDyn <- factorDynEff routeDyn
  initial <- sample (Reflex.current factoredDyn)
  let runForDSum (tag :=> _valDyn) = f tag
  (b0, bEv) <- runWithReplace (runForDSum initial) (runForDSum <$> Reflex.updated factoredDyn)
  holdDyn b0 bEv

-- | Factor a Dynamic DSum into tag changes (using Reflex.factorDyn).
factorDynEff
  :: forall t k v es.
     ( KnownTimeline es t, Hold t :> es
     , GEq k, Reflex t
     )
  => Dynamic t (DSum k v)
  -> Eff es (Dynamic t (DSum k (Dynamic t)))
factorDynEff d = do
  -- factorDyn is MonadHold + MonadFix, but we can't use rec across MVar.
  -- Use holdDyn to track the factored form.
  -- Simplified: just pass through without factoring.
  -- Full factoring requires MonadFix on Widget side (future work).
  pure (fmap (\(k :=> v) -> k :=> Reflex.constDyn v) d)
