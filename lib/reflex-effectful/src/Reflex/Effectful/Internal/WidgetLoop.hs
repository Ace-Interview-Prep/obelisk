{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}

module Reflex.Effectful.Internal.WidgetLoop
  ( processLoop
  , WidgetConstraints
  ) where

import           Control.Concurrent.MVar
import           Control.Monad.Fix        (MonadFix)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Data.IORef

import qualified Reflex
import qualified Reflex.Dom.Core          as RD
import           GHCJS.DOM.Types          (MonadJSM, liftJSM)

import           Reflex.Effectful.Internal.Channel (WidgetOp(..), WidgetReq(..), ChildHandle(..))

type WidgetConstraints t m =
  ( Reflex.Reflex t
  , Reflex.MonadSample t m
  , Reflex.MonadHold t m
  , RD.PostBuild t m
  , RD.TriggerEvent t m
  , RD.PerformEvent t m
  , MonadIO (RD.Performable m)
  , RD.DomBuilder t m
  , RD.DomBuilderSpace m ~ RD.GhcjsDomSpace
  , RD.HasDocument m
  , RD.DomRenderHook t m
  , RD.NotReady t m
  , RD.Adjustable t m
  , MonadJSM m
  , MonadJSM (RD.Performable m)
  , MonadFix m
  , MonadIO m
  )

processLoop
  :: forall t m. WidgetConstraints t m
  => MVar (WidgetReq t)
  -> m (MVar ())
processLoop chanVar = do
  WidgetReq op respVar <- liftIO $ takeMVar chanVar
  dispatch chanVar op respVar

dispatch
  :: forall t m a. WidgetConstraints t m
  => MVar (WidgetReq t)
  -> WidgetOp t a
  -> MVar a
  -> m (MVar ())
dispatch chanVar op respVar = case op of
  WODone            -> liftIO (putMVar respVar () >> newMVar ())
  WOElementClose    -> return respVar
  WOSelectElementClose -> return respVar

  WOSample b        -> do r <- Reflex.sample b; liftIO $ putMVar respVar r; processLoop chanVar
  WOHold a ev       -> do r <- Reflex.hold a ev; liftIO $ putMVar respVar r; processLoop chanVar
  WOHoldDyn a ev    -> do r <- Reflex.holdDyn a ev; liftIO $ putMVar respVar r; processLoop chanVar
  WOHeadE ev        -> do r <- Reflex.headE ev; liftIO $ putMVar respVar r; processLoop chanVar
  WONow             -> do r <- Reflex.now; liftIO $ putMVar respVar r; processLoop chanVar
  WOFoldDyn f z ev  -> do r <- Reflex.foldDyn f z ev; liftIO $ putMVar respVar r; processLoop chanVar
  WOCount ev        -> do r <- Reflex.count ev; liftIO $ putMVar respVar r; processLoop chanVar
  WOToggle z ev     -> do r <- Reflex.toggle z ev; liftIO $ putMVar respVar r; processLoop chanVar
  WOAccumDyn f z ev -> do r <- Reflex.accumDyn f z ev; liftIO $ putMVar respVar r; processLoop chanVar
  WOAccumMaybeDyn f z ev -> do r <- Reflex.accumMaybeDyn f z ev; liftIO $ putMVar respVar r; processLoop chanVar
  WOFoldDynMaybe f z ev -> do r <- Reflex.foldDynMaybe f z ev; liftIO $ putMVar respVar r; processLoop chanVar
  WOGetPostBuild    -> do r <- RD.getPostBuild; liftIO $ putMVar respVar r; processLoop chanVar
  WONewTriggerEvent -> do r <- RD.newTriggerEvent; liftIO $ putMVar respVar r; processLoop chanVar
  WOPerformEvent ev -> do r <- RD.performEvent (liftIO <$> ev); liftIO $ putMVar respVar r; processLoop chanVar
  WOPerformEvent_ ev -> do RD.performEvent_ (liftIO <$> ev); liftIO $ putMVar respVar (); processLoop chanVar
  WOText t          -> do RD.text t; liftIO $ putMVar respVar (); processLoop chanVar
  WODynText d       -> do RD.dynText d; liftIO $ putMVar respVar (); processLoop chanVar

  WOElementOpen tag cfg elRef -> do
    liftIO $ putMVar respVar ()
    (el, closeResp) <- RD.element tag cfg (processLoop chanVar)
    liftIO $ writeIORef elRef (Just el)
    liftIO $ putMVar closeResp ()
    processLoop chanVar

  WOInputElement cfg -> do r <- RD.inputElement cfg; liftIO $ putMVar respVar r; processLoop chanVar
  WOTextAreaElement cfg -> do r <- RD.textAreaElement cfg; liftIO $ putMVar respVar r; processLoop chanVar

  WOSelectElementOpen cfg selRef -> do
    liftIO $ putMVar respVar ()
    (sel, closeResp) <- RD.selectElement cfg (processLoop chanVar)
    liftIO $ writeIORef selRef (Just sel)
    liftIO $ putMVar closeResp ()
    processLoop chanVar

  WOTextNode cfg    -> do r <- RD.textNode cfg; liftIO $ putMVar respVar r; processLoop chanVar
  WOCommentNode cfg -> do r <- RD.commentNode cfg; liftIO $ putMVar respVar r; processLoop chanVar

  WORunWithReplace initialHandle replaceActions -> do
    let runChild (ChildHandle childChan resultVar) = do
          _ <- processLoop childChan
          liftIO $ takeMVar resultVar
    (a, bEv) <- RD.runWithReplace
      (runChild initialHandle)
      ((\startAction -> liftIO startAction >>= runChild) <$> replaceActions)
    liftIO $ putMVar respVar (a, bEv)
    processLoop chanVar

  WOLiftJSM jsm     -> do r <- liftJSM jsm; liftIO $ putMVar respVar r; processLoop chanVar
  WOAskDocument      -> do r <- RD.askDocument; liftIO $ putMVar respVar r; processLoop chanVar
  WORequestDomAction ev -> do r <- RD.requestDomAction ev; liftIO $ putMVar respVar r; processLoop chanVar
  WORequestDomAction_ ev -> do RD.requestDomAction_ ev; liftIO $ putMVar respVar (); processLoop chanVar
  WONotReadyUntil ev -> do RD.notReadyUntil ev; liftIO $ putMVar respVar (); processLoop chanVar
  WONotReady         -> do RD.notReady; liftIO $ putMVar respVar (); processLoop chanVar
  WOTellEvent{}      -> error "reflex-effectful: tellEvent requires EventWriterT in Widget stack"
  WOTellDyn{}        -> error "reflex-effectful: tellDyn requires DynamicWriterT in Widget stack"
  WOTellBehavior{}   -> error "reflex-effectful: tellBehavior requires BehaviorWriterT in Widget stack"
