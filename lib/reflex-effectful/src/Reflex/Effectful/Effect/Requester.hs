{-# LANGUAGE AllowAmbiguousTypes #-}

module Reflex.Effectful.Effect.Requester (Requester(..), requesting, requesting_) where

import           Data.Dependent.Map       (DMap)
import           Data.GADT.Compare        (GCompare)
import           Effectful                (Effect, Dispatch(..), DispatchOf, (:>), Eff)
import           Effectful.Dispatch.Dynamic (send)
import           Reflex                   (Event)

data Requester t req rsp :: Effect where
  Requesting  :: GCompare k => Event t (DMap k req) -> Requester t req rsp m (Event t (DMap k rsp))
  Requesting_ :: GCompare k => Event t (DMap k req) -> Requester t req rsp m ()

type instance DispatchOf (Requester t req rsp) = 'Dynamic

requesting :: (Requester t req rsp :> es, GCompare k) => Event t (DMap k req) -> Eff es (Event t (DMap k rsp))
requesting = send . Requesting

requesting_ :: forall t req rsp es k. (Requester t req rsp :> es, GCompare k) => Event t (DMap k req) -> Eff es ()
requesting_ = send @(Requester t req rsp) . Requesting_
