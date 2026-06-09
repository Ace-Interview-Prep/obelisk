module Reflex.Effectful.Effect.PerformEvent
  ( PerformEvent(..)
  , performEvent
  , performEvent_
  , performRequestAsync
  ) where

import           Effectful                (Effect, Dispatch(..), DispatchOf, (:>), Eff)
import           Effectful.Dispatch.Dynamic (send)
import           GHCJS.DOM.Types          (JSM)
import           Reflex.Effectful.Types   (KnownTimeline)
import           Reflex                   (Event)
import qualified Reflex.Dom.Xhr           as Xhr

data PerformEvent t :: Effect where
  PerformEvent  :: Event t (JSM a) -> PerformEvent t m (Event t a)
  PerformEvent_ :: Event t (JSM ()) -> PerformEvent t m ()
  PerformRequestAsync :: Event t (Xhr.XhrRequest ()) -> PerformEvent t m (Event t Xhr.XhrResponse)

type instance DispatchOf (PerformEvent t) = 'Dynamic

performEvent :: (KnownTimeline es t, PerformEvent t :> es) => Event t (JSM a) -> Eff es (Event t a)
performEvent = send . PerformEvent

performEvent_ :: (KnownTimeline es t, PerformEvent t :> es) => Event t (JSM ()) -> Eff es ()
performEvent_ = send . PerformEvent_

performRequestAsync :: (KnownTimeline es t, PerformEvent t :> es) => Event t (Xhr.XhrRequest ()) -> Eff es (Event t Xhr.XhrResponse)
performRequestAsync = send . PerformRequestAsync
