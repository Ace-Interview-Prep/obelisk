module Reflex.Effectful.Effect.BehaviorWriter (BehaviorWriter(..), tellBehavior) where

import           Effectful                (Effect, Dispatch(..), DispatchOf, (:>), Eff)
import           Effectful.Dispatch.Dynamic (send)
import           Reflex                   (Behavior)

data BehaviorWriter t w :: Effect where
  TellBehavior :: Behavior t w -> BehaviorWriter t w m ()

type instance DispatchOf (BehaviorWriter t w) = 'Dynamic

tellBehavior :: (BehaviorWriter t w :> es, Monoid w) => Behavior t w -> Eff es ()
tellBehavior = send . TellBehavior
