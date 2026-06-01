module Reflex.Effectful.Effect.DynamicWriter (DynamicWriter(..), tellDyn) where

import           Effectful                (Effect, Dispatch(..), DispatchOf, (:>), Eff)
import           Effectful.Dispatch.Dynamic (send)
import           Reflex                   (Dynamic)

data DynamicWriter t w :: Effect where
  TellDyn :: Dynamic t w -> DynamicWriter t w m ()

type instance DispatchOf (DynamicWriter t w) = 'Dynamic

tellDyn :: (DynamicWriter t w :> es, Monoid w) => Dynamic t w -> Eff es ()
tellDyn = send . TellDyn
