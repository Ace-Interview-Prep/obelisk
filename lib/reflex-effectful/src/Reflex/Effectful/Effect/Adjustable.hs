module Reflex.Effectful.Effect.Adjustable
  ( Adjustable(..)
  , runWithReplace, traverseIntMapWithKeyWithAdjust
  , traverseDMapWithKeyWithAdjust, traverseDMapWithKeyWithAdjustWithMove
  , widgetHold, widgetHold_, dyn, dyn_
  ) where

import           Data.Dependent.Map       (DMap)
import qualified Data.IntMap              as IntMap
import           Data.GADT.Compare        (GCompare)
import           Data.IntMap              (IntMap)

import           Effectful                (Effect, Dispatch(..), DispatchOf, (:>), Eff)
import           Effectful.Dispatch.Dynamic (send)
import           Reflex.Effectful.Types   (KnownTimeline)

import           Reflex                   (Event, Dynamic, PatchDMap, PatchDMapWithMove, PatchIntMap, Reflex)
import qualified Reflex                   as R
import           Reflex.Effectful.Effect.Dom (Dom, blank)
import           Reflex.Effectful.Effect.Hold (Hold, holdDyn)

data Adjustable t :: Effect where
  RunWithReplace
    :: m a -> Event t (m b) -> Adjustable t m (a, Event t b)
  TraverseIntMapWithKeyWithAdjust
    :: (IntMap.Key -> v -> m v') -> IntMap v -> Event t (PatchIntMap v) -> Adjustable t m (IntMap v', Event t (PatchIntMap v'))
  TraverseDMapWithKeyWithAdjust
    :: GCompare k => (forall a. k a -> v a -> m (v' a)) -> DMap k v -> Event t (PatchDMap k v) -> Adjustable t m (DMap k v', Event t (PatchDMap k v'))
  TraverseDMapWithKeyWithAdjustWithMove
    :: GCompare k => (forall a. k a -> v a -> m (v' a)) -> DMap k v -> Event t (PatchDMapWithMove k v) -> Adjustable t m (DMap k v', Event t (PatchDMapWithMove k v'))

type instance DispatchOf (Adjustable t) = 'Dynamic

runWithReplace :: (KnownTimeline es t, Adjustable t :> es) => Eff es a -> Event t (Eff es b) -> Eff es (a, Event t b)
runWithReplace initial ev = send (RunWithReplace initial ev)

traverseIntMapWithKeyWithAdjust :: (KnownTimeline es t, Adjustable t :> es) => (IntMap.Key -> v -> Eff es v') -> IntMap v -> Event t (PatchIntMap v) -> Eff es (IntMap v', Event t (PatchIntMap v'))
traverseIntMapWithKeyWithAdjust f im ev = send (TraverseIntMapWithKeyWithAdjust f im ev)

traverseDMapWithKeyWithAdjust :: (KnownTimeline es t, Adjustable t :> es, GCompare k) => (forall a. k a -> v a -> Eff es (v' a)) -> DMap k v -> Event t (PatchDMap k v) -> Eff es (DMap k v', Event t (PatchDMap k v'))
traverseDMapWithKeyWithAdjust f dm ev = send (TraverseDMapWithKeyWithAdjust f dm ev)

traverseDMapWithKeyWithAdjustWithMove :: (KnownTimeline es t, Adjustable t :> es, GCompare k) => (forall a. k a -> v a -> Eff es (v' a)) -> DMap k v -> Event t (PatchDMapWithMove k v) -> Eff es (DMap k v', Event t (PatchDMapWithMove k v'))
traverseDMapWithKeyWithAdjustWithMove f dm ev = send (TraverseDMapWithKeyWithAdjustWithMove f dm ev)

widgetHold :: (KnownTimeline es t, Adjustable t :> es, Hold t :> es) => Eff es a -> Event t (Eff es a) -> Eff es (Dynamic t a)
widgetHold initial ev = do { (a0, a') <- runWithReplace initial ev; holdDyn a0 a' }

widgetHold_ :: (KnownTimeline es t, Adjustable t :> es) => Eff es () -> Event t (Eff es ()) -> Eff es ()
widgetHold_ initial ev = do { _ <- runWithReplace initial ev; return () }

dyn :: (KnownTimeline es t, Adjustable t :> es, Hold t :> es, Dom t :> es, Reflex t) => Dynamic t (Eff es a) -> Eff es (Event t a)
dyn d = do { (_, ev) <- runWithReplace blank (R.updated d); return ev }

dyn_ :: (KnownTimeline es t, Adjustable t :> es, Dom t :> es, Reflex t) => Dynamic t (Eff es ()) -> Eff es ()
dyn_ d = do { _ <- runWithReplace blank (R.updated d); return () }
