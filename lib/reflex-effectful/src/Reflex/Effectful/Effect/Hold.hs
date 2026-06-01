module Reflex.Effectful.Effect.Hold
  ( Hold(..)
  , hold, holdDyn, holdIncremental, buildDynamic, headE, now
  , foldDyn, count, toggle, accumDyn, accumMaybeDyn, foldDynMaybe
  ) where

import           Effectful                (Effect, Dispatch(..), DispatchOf, (:>), Eff)
import           Effectful.Dispatch.Dynamic (send)
import           Reflex.Effectful.Types   (KnownTimeline)
import           Reflex                   (Behavior, Dynamic, Event, Incremental, Patch, PatchTarget, PushM)

data Hold t :: Effect where
  Hold            :: a -> Event t a -> Hold t m (Behavior t a)
  HoldDyn         :: a -> Event t a -> Hold t m (Dynamic t a)
  HoldIncremental :: Patch p => PatchTarget p -> Event t p -> Hold t m (Incremental t p)
  BuildDynamic    :: PushM t a -> Event t a -> Hold t m (Dynamic t a)
  HeadE           :: Event t a -> Hold t m (Event t a)
  Now             :: Hold t m (Event t ())
  FoldDyn         :: (a -> b -> b) -> b -> Event t a -> Hold t m (Dynamic t b)
  Count           :: Num b => Event t a -> Hold t m (Dynamic t b)
  Toggle          :: Bool -> Event t a -> Hold t m (Dynamic t Bool)
  AccumDyn        :: (a -> b -> a) -> a -> Event t b -> Hold t m (Dynamic t a)
  AccumMaybeDyn   :: (a -> b -> Maybe a) -> a -> Event t b -> Hold t m (Dynamic t a)
  FoldDynMaybe    :: (a -> b -> Maybe b) -> b -> Event t a -> Hold t m (Dynamic t b)

type instance DispatchOf (Hold t) = 'Dynamic

hold :: (KnownTimeline es t, Hold t :> es) => a -> Event t a -> Eff es (Behavior t a)
hold a ev = send (Hold a ev)

holdDyn :: (KnownTimeline es t, Hold t :> es) => a -> Event t a -> Eff es (Dynamic t a)
holdDyn a ev = send (HoldDyn a ev)

holdIncremental :: (Hold t :> es, Patch p) => PatchTarget p -> Event t p -> Eff es (Incremental t p)
holdIncremental a ev = send (HoldIncremental a ev)

buildDynamic :: (KnownTimeline es t, Hold t :> es) => PushM t a -> Event t a -> Eff es (Dynamic t a)
buildDynamic p ev = send (BuildDynamic p ev)

headE :: (KnownTimeline es t, Hold t :> es) => Event t a -> Eff es (Event t a)
headE = send . HeadE

now :: (KnownTimeline es t, Hold t :> es) => Eff es (Event t ())
now = send Now

foldDyn :: (KnownTimeline es t, Hold t :> es) => (a -> b -> b) -> b -> Event t a -> Eff es (Dynamic t b)
foldDyn f z ev = send (FoldDyn f z ev)

count :: (Hold t :> es, Num b) => Event t a -> Eff es (Dynamic t b)
count = send . Count

toggle :: (KnownTimeline es t, Hold t :> es) => Bool -> Event t a -> Eff es (Dynamic t Bool)
toggle z ev = send (Toggle z ev)

accumDyn :: (KnownTimeline es t, Hold t :> es) => (a -> b -> a) -> a -> Event t b -> Eff es (Dynamic t a)
accumDyn f z ev = send (AccumDyn f z ev)

accumMaybeDyn :: (KnownTimeline es t, Hold t :> es) => (a -> b -> Maybe a) -> a -> Event t b -> Eff es (Dynamic t a)
accumMaybeDyn f z ev = send (AccumMaybeDyn f z ev)

foldDynMaybe :: (KnownTimeline es t, Hold t :> es) => (a -> b -> Maybe b) -> b -> Event t a -> Eff es (Dynamic t b)
foldDynMaybe f z ev = send (FoldDynMaybe f z ev)
