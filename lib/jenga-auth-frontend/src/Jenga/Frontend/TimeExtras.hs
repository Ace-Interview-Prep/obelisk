{-# LANGUAGE TupleSections #-}

module Jenga.Frontend.TimeExtras where

import Jenga.Frontend.JS (Trace)
import Reflex
import Reflex.Dom.Core (tickLossyFrom')
import Control.Monad (forM)
import Data.Time

import Effectful (Eff, (:>), IOE)
import Reflex.Effectful.Effect.Hold (Hold, holdDyn, foldDyn, count)
import Reflex.Effectful.Effect.PerformEvent (PerformEvent, performEvent)
import Reflex.Effectful.Effect.TriggerEvent (TriggerEvent)
import Reflex.Effectful.Effect.PostBuild (PostBuild, getPostBuild)
import Reflex.Effectful.Effect.Dom (Dom)
import Control.Monad.IO.Class (liftIO)

timer :: ( PerformEvent t :> es
         , Hold t :> es
         , TriggerEvent t :> es
         , IOE :> es
         , Reflex t
         )
      => Event t ()
      -> Event t ()
      -> Event t ()
      -> Eff es (Dynamic t NominalDiffTime)
timer start stop reset = do
  startTimeEv <- performEvent $ liftIO getCurrentTime <$ start
  mStartTime <- holdDyn Nothing $ leftmost
    [ Just <$> startTimeEv
    , Nothing <$ stop
    , Nothing <$ reset
    ]
  firstStart <- headE startTimeEv
  tick <- tickLossyFrom' $ (0.1,) <$> firstStart
  let elapsed = attachWith (\mStart tickInfo ->
        case mStart of
          Nothing -> 0
          Just s  -> diffUTCTime (_tickInfo_lastUTC tickInfo) s
        ) (current mStartTime) tick
  holdDyn 0 elapsed

tickLossyFrom'' :: ( PerformEvent t :> es
                   , TriggerEvent t :> es
                   , IOE :> es
                   , Reflex t
                   ) => NominalDiffTime -> Event t a -> Eff es (Event t TickInfo)
tickLossyFrom'' nomnom ev = do
  eventTime <- performEvent $ liftIO getCurrentTime <$ ev
  tickLossyFrom' $ (nomnom,) <$> eventTime

countTimeFrom :: ( PerformEvent t :> es
                 , TriggerEvent t :> es
                 , Hold t :> es
                 , IOE :> es
                 , Reflex t
                 ) => NominalDiffTime -> Event t a -> Eff es (Event t NominalDiffTime)
countTimeFrom interval ev = do
  eventTime <- performEvent $ liftIO getCurrentTime <$ ev
  eventTimeDyn <- foldDyn const undefined eventTime
  tick <- tickLossyFrom' $ (interval,) <$> eventTime
  pure $ attachWith (\start' now_ -> diffUTCTime (_tickInfo_lastUTC now_) start') (current eventTimeDyn) tick

hasBeen :: Reflex t => NominalDiffTime -> Dynamic t NominalDiffTime -> Event t ()
hasBeen threshold tElapsed = mapMaybe (\t_ -> if t_ > threshold then Just () else Nothing) (updated tElapsed)

hasBeenRange :: Reflex t => (NominalDiffTime, NominalDiffTime) -> Dynamic t NominalDiffTime -> Event t ()
hasBeenRange (low, high) tElapsed = mapMaybe (\t_ -> if t_ > low && t_ < high then Just () else Nothing) (updated tElapsed)

getPostBuildDelayed
  :: ( PostBuild t :> es
     , PerformEvent t :> es
     , TriggerEvent t :> es
     , Hold t :> es
     , IOE :> es
     , Reflex t
     )
  => [NominalDiffTime]
  -> Eff es (Event t ())
getPostBuildDelayed renderTimes = do
  pBuild <- getPostBuild
  pbDs <- forM renderTimes $ \t_ -> do
    delay t_ pBuild
  fmap (() <$) $ headE $ leftmost pbDs
