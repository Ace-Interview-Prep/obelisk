module Reflex.Effectful.Effect.Sample
  ( Sample(..)
  , sample
  ) where

import           Effectful                (Effect, Dispatch(..), DispatchOf, (:>), Eff)
import           Effectful.Dispatch.Dynamic (send)
import           Reflex.Effectful.Types   (KnownTimeline)
import           Reflex                   (Behavior)

data Sample t :: Effect where
  Sample :: Behavior t a -> Sample t m a

type instance DispatchOf (Sample t) = 'Dynamic

sample :: (KnownTimeline es t, Sample t :> es) => Behavior t a -> Eff es a
sample = send . Sample
