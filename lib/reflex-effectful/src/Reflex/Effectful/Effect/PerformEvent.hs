module Reflex.Effectful.Effect.PerformEvent
  ( PerformEvent(..)
  , performEvent
  , performEvent_
  ) where

import           Effectful                (Effect, Dispatch(..), DispatchOf, (:>), Eff)
import           Effectful.Dispatch.Dynamic (send)
import           Reflex.Effectful.Types   (KnownTimeline)
import           Reflex                   (Event)

data PerformEvent t :: Effect where
  PerformEvent  :: Event t (IO a) -> PerformEvent t m (Event t a)
  PerformEvent_ :: Event t (IO ()) -> PerformEvent t m ()

type instance DispatchOf (PerformEvent t) = 'Dynamic

performEvent :: (KnownTimeline es t, PerformEvent t :> es) => Event t (IO a) -> Eff es (Event t a)
performEvent = send . PerformEvent

performEvent_ :: (KnownTimeline es t, PerformEvent t :> es) => Event t (IO ()) -> Eff es ()
performEvent_ = send . PerformEvent_
