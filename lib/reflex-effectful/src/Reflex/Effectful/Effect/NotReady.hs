{-# LANGUAGE AllowAmbiguousTypes #-}

module Reflex.Effectful.Effect.NotReady (NotReady(..), notReadyUntil, notReady) where

import           Effectful                (Effect, Dispatch(..), DispatchOf, (:>), Eff)
import           Effectful.Dispatch.Dynamic (send)
import           Reflex.Effectful.Types   (KnownTimeline)
import           Reflex                   (Event)

data NotReady t :: Effect where
  NotReadyUntil :: Event t a -> NotReady t m ()
  NotReady_     :: NotReady t m ()

type instance DispatchOf (NotReady t) = 'Dynamic

notReadyUntil :: (KnownTimeline es t, NotReady t :> es) => Event t a -> Eff es ()
notReadyUntil = send . NotReadyUntil

notReady :: forall t es. NotReady t :> es => Eff es ()
notReady = send @(NotReady t) NotReady_
