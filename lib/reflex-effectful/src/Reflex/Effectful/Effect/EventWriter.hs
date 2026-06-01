module Reflex.Effectful.Effect.EventWriter (EventWriter(..), tellEvent) where

import           Effectful                (Effect, Dispatch(..), DispatchOf, (:>), Eff)
import           Effectful.Dispatch.Dynamic (send)
import           Reflex                   (Event)

data EventWriter t w :: Effect where
  TellEvent :: Event t w -> EventWriter t w m ()

type instance DispatchOf (EventWriter t w) = 'Dynamic

tellEvent :: (EventWriter t w :> es, Semigroup w) => Event t w -> Eff es ()
tellEvent = send . TellEvent
