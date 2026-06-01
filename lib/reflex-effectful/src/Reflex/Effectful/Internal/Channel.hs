{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reflex.Effectful.Internal.Channel
  ( WidgetChannel
  , sendReq
  , sendReqIO
  , runWidgetChannel
  , WidgetOp(..)
  , WidgetReq(..)
  , ChildHandle(..)
  , ChannelRegistry(..)
  , startChild
  , getRegistry
  ) where

import           Control.Concurrent       (forkIO, myThreadId, ThreadId)
import           Control.Concurrent.MVar
import           Data.IORef
import qualified Data.Map.Strict          as Map
import           Data.Text                (Text)

import           Effectful
import           Effectful.Dispatch.Static

import           Reflex                   (Behavior, Dynamic, Event, Incremental)
import           GHCJS.DOM.Types          (JSM)
import qualified Reflex.Dom.Builder.Class as DBC

import           Reflex.Effectful.Types

data ChannelRegistry t = ChannelRegistry
  { _cr_channels :: IORef (Map.Map ThreadId (MVar (WidgetReq t)))
  }

data WidgetOp t result where
  WOSample          :: Behavior t a -> WidgetOp t a
  WOHold            :: a -> Event t a -> WidgetOp t (Behavior t a)
  WOHoldDyn         :: a -> Event t a -> WidgetOp t (Dynamic t a)
  WOHeadE           :: Event t a -> WidgetOp t (Event t a)
  WONow             :: WidgetOp t (Event t ())
  WOFoldDyn         :: (a -> b -> b) -> b -> Event t a -> WidgetOp t (Dynamic t b)
  WOCount           :: Num b => Event t a -> WidgetOp t (Dynamic t b)
  WOToggle          :: Bool -> Event t a -> WidgetOp t (Dynamic t Bool)
  WOAccumDyn        :: (a -> b -> a) -> a -> Event t b -> WidgetOp t (Dynamic t a)
  WOAccumMaybeDyn   :: (a -> b -> Maybe a) -> a -> Event t b -> WidgetOp t (Dynamic t a)
  WOFoldDynMaybe    :: (a -> b -> Maybe b) -> b -> Event t a -> WidgetOp t (Dynamic t b)
  WOGetPostBuild    :: WidgetOp t (Event t ())
  WONewTriggerEvent :: WidgetOp t (Event t a, a -> IO ())
  WOPerformEvent    :: Event t (IO a) -> WidgetOp t (Event t a)
  WOPerformEvent_   :: Event t (IO ()) -> WidgetOp t ()
  WOText            :: Text -> WidgetOp t ()
  WODynText         :: Dynamic t Text -> WidgetOp t ()
  WOElementOpen     :: Text -> ElementConfig EventResult t GhcjsDomSpace -> IORef (Maybe (Element EventResult GhcjsDomSpace t)) -> WidgetOp t ()
  WOElementClose    :: WidgetOp t ()
  WOInputElement    :: InputElementConfig EventResult t GhcjsDomSpace -> WidgetOp t (InputElement EventResult GhcjsDomSpace t)
  WOTextAreaElement :: TextAreaElementConfig EventResult t GhcjsDomSpace -> WidgetOp t (TextAreaElement EventResult GhcjsDomSpace t)
  WOSelectElementOpen :: SelectElementConfig EventResult t GhcjsDomSpace -> IORef (Maybe (SelectElement EventResult GhcjsDomSpace t)) -> WidgetOp t ()
  WOSelectElementClose :: WidgetOp t ()
  WOTextNode        :: TextNodeConfig t -> WidgetOp t (TextNode GhcjsDomSpace t)
  WOCommentNode     :: CommentNodeConfig t -> WidgetOp t (CommentNode GhcjsDomSpace t)
  WORunWithReplace   :: ChildHandle t a -> Event t (IO (ChildHandle t b)) -> WidgetOp t (a, Event t b)
  WOLiftJSM         :: JSM a -> WidgetOp t a
  WOAskDocument     :: WidgetOp t (DBC.RawDocument GhcjsDomSpace)
  WORequestDomAction  :: Event t (JSM a) -> WidgetOp t (Event t a)
  WORequestDomAction_ :: Event t (JSM a) -> WidgetOp t ()
  WONotReadyUntil   :: Event t a -> WidgetOp t ()
  WONotReady        :: WidgetOp t ()
  WOTellEvent       :: Event t a -> WidgetOp t ()
  WOTellDyn         :: Dynamic t a -> WidgetOp t ()
  WOTellBehavior    :: Behavior t a -> WidgetOp t ()
  WODone            :: WidgetOp t ()

data WidgetReq t where
  WidgetReq :: WidgetOp t a -> MVar a -> WidgetReq t

data ChildHandle t a = ChildHandle
  { _ch_channel :: MVar (WidgetReq t)
  , _ch_result  :: MVar a
  }

data WidgetChannel t :: Effect
type instance DispatchOf (WidgetChannel t) = 'Static 'WithSideEffects
newtype instance StaticRep (WidgetChannel t) = WidgetChannelRep (ChannelRegistry t)

sendReq :: WidgetChannel t :> es => WidgetOp t a -> Eff es a
sendReq op = do
  WidgetChannelRep reg <- getStaticRep
  unsafeEff_ $ sendReqIO reg op

sendReqIO :: ChannelRegistry t -> WidgetOp t a -> IO a
sendReqIO reg op = do
  tid <- myThreadId
  chanMap <- readIORef (_cr_channels reg)
  case Map.lookup tid chanMap of
    Nothing -> error "reflex-effectful: no channel registered for this thread"
    Just chanVar -> do
      respVar <- newEmptyMVar
      putMVar chanVar (WidgetReq op respVar)
      takeMVar respVar

runWidgetChannel :: IOE :> es => MVar (WidgetReq t) -> Eff (WidgetChannel t : es) a -> Eff es a
runWidgetChannel chanVar eff = do
  reg <- unsafeEff_ $ do
    tid <- myThreadId
    mapRef <- newIORef (Map.singleton tid chanVar)
    return (ChannelRegistry mapRef)
  evalStaticRep (WidgetChannelRep reg) eff

getRegistry :: WidgetChannel t :> es => Eff es (ChannelRegistry t)
getRegistry = do
  WidgetChannelRep reg <- getStaticRep
  return reg

startChild :: ChannelRegistry t -> IO a -> IO (ChildHandle t a)
startChild reg childIO = do
  childChan <- newEmptyMVar
  resultVar <- newEmptyMVar
  _ <- forkIO $ do
    tid <- myThreadId
    atomicModifyIORef' (_cr_channels reg) (\m -> (Map.insert tid childChan m, ()))
    a <- childIO
    putMVar resultVar a
    doneResp <- newEmptyMVar
    putMVar childChan (WidgetReq WODone doneResp)
    takeMVar doneResp
    atomicModifyIORef' (_cr_channels reg) (\m -> (Map.delete tid m, ()))
  return (ChildHandle childChan resultVar)
