module Reflex.Effectful.Effect.TriggerEvent
  ( TriggerEvent(..)
  , newTriggerEvent
  , newTriggerEventWithOnComplete
  , delay
  , tickLossyFrom'
  ) where

import           Data.Time.Clock          (NominalDiffTime, UTCTime)
import           Effectful                (Effect, Dispatch(..), DispatchOf, (:>), Eff)
import           Effectful.Dispatch.Dynamic (send)
import           Reflex.Effectful.Types   (KnownTimeline)
import           Reflex                   (Event, TickInfo)

data TriggerEvent t :: Effect where
  NewTriggerEvent :: TriggerEvent t m (Event t a, a -> IO ())
  NewTriggerEventWithOnComplete :: TriggerEvent t m (Event t a, a -> IO () -> IO ())
  Delay :: NominalDiffTime -> Event t a -> TriggerEvent t m (Event t a)
  TickLossyFrom :: Event t (NominalDiffTime, UTCTime) -> TriggerEvent t m (Event t TickInfo)

type instance DispatchOf (TriggerEvent t) = 'Dynamic

newTriggerEvent :: (KnownTimeline es t, TriggerEvent t :> es) => Eff es (Event t a, a -> IO ())
newTriggerEvent = send NewTriggerEvent

newTriggerEventWithOnComplete :: (KnownTimeline es t, TriggerEvent t :> es) => Eff es (Event t a, a -> IO () -> IO ())
newTriggerEventWithOnComplete = send NewTriggerEventWithOnComplete

delay :: (KnownTimeline es t, TriggerEvent t :> es) => NominalDiffTime -> Event t a -> Eff es (Event t a)
delay dt ev = send (Delay dt ev)

tickLossyFrom' :: (KnownTimeline es t, TriggerEvent t :> es) => Event t (NominalDiffTime, UTCTime) -> Eff es (Event t TickInfo)
tickLossyFrom' ev = send (TickLossyFrom ev)
