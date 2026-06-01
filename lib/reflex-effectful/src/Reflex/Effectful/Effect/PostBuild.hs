module Reflex.Effectful.Effect.PostBuild
  ( PostBuild(..)
  , getPostBuild
  ) where

import           Effectful                (Effect, Dispatch(..), DispatchOf, (:>), Eff)
import           Effectful.Dispatch.Dynamic (send)
import           Reflex                   (Event)
import           Reflex.Effectful.Types   (KnownTimeline)

data PostBuild t :: Effect where
  GetPostBuild :: PostBuild t m (Event t ())

type instance DispatchOf (PostBuild t) = 'Dynamic

getPostBuild :: (KnownTimeline es t, PostBuild t :> es) => Eff es (Event t ())
getPostBuild = send GetPostBuild
