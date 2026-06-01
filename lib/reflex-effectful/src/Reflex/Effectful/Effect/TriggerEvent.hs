module Reflex.Effectful.Effect.TriggerEvent
  ( TriggerEvent(..)
  , newTriggerEvent
  , newTriggerEventWithOnComplete
  ) where

import           Effectful                (Effect, Dispatch(..), DispatchOf, (:>), Eff)
import           Effectful.Dispatch.Dynamic (send)
import           Reflex.Effectful.Types   (KnownTimeline)
import           Reflex                   (Event)

data TriggerEvent t :: Effect where
  NewTriggerEvent :: TriggerEvent t m (Event t a, a -> IO ())
  NewTriggerEventWithOnComplete :: TriggerEvent t m (Event t a, a -> IO () -> IO ())

type instance DispatchOf (TriggerEvent t) = 'Dynamic

newTriggerEvent :: (KnownTimeline es t, TriggerEvent t :> es) => Eff es (Event t a, a -> IO ())
newTriggerEvent = send NewTriggerEvent

newTriggerEventWithOnComplete :: (KnownTimeline es t, TriggerEvent t :> es) => Eff es (Event t a, a -> IO () -> IO ())
newTriggerEventWithOnComplete = send NewTriggerEventWithOnComplete
