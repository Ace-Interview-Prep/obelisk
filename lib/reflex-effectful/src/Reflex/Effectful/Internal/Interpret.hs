{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reflex.Effectful.Internal.Interpret
  ( interpretSample, interpretHold, interpretPostBuild
  , interpretTriggerEvent, interpretPerformEvent, interpretDom
  , interpretAdjustable, interpretPrerender
  , interpretJSM, interpretHasDocument, interpretDomRenderHook
  , interpretNotReady
  , interpretEventWriter, interpretDynamicWriter, interpretBehaviorWriter
  , interpretAll
  ) where

import           Data.IORef
import           Effectful
import           Effectful.Dispatch.Dynamic
import qualified Reflex

import           Reflex.Effectful.Effect.Sample           (Sample(..))
import           Reflex.Effectful.Effect.Hold             (Hold(..))
import           Reflex.Effectful.Effect.PostBuild        (PostBuild(..))
import           Reflex.Effectful.Effect.TriggerEvent     (TriggerEvent(..))
import           Reflex.Effectful.Effect.PerformEvent     (PerformEvent(..))
import           Reflex.Effectful.Effect.Dom              (Dom(..))
import           Reflex.Effectful.Effect.Adjustable       (Adjustable(..))
import           Reflex.Effectful.Effect.Prerender        (Prerender(..))
import           Reflex.Effectful.Effect.JSM              (JSM'(..))
import           Reflex.Effectful.Effect.HasDocument      (HasDocument(..))
import           Reflex.Effectful.Effect.DomRenderHook    (DomRenderHook(..))
import           Reflex.Effectful.Effect.NotReady         (NotReady(..))
import           Reflex.Effectful.Effect.EventWriter      (EventWriter(..))
import           Reflex.Effectful.Effect.DynamicWriter    (DynamicWriter(..))
import           Reflex.Effectful.Effect.BehaviorWriter   (BehaviorWriter(..))
import           Reflex.Effectful.Internal.Channel

-- Helper to disambiguate t for timeline-independent WidgetOp constructors
sr :: forall t r es. WidgetChannel t :> es => WidgetOp t r -> Eff es r
sr = sendReq

interpretSample :: forall t es a. WidgetChannel t :> es => Eff (Sample t : es) a -> Eff es a
interpretSample = interpret_ $ \case
  Sample b -> sendReq (WOSample b)

interpretHold :: forall t es a. WidgetChannel t :> es => Eff (Hold t : es) a -> Eff es a
interpretHold = interpret_ $ \case
  Hold a ev           -> sendReq (WOHold a ev)
  HoldDyn a ev        -> sendReq (WOHoldDyn a ev)
  HoldIncremental{}   -> error "reflex-effectful: holdIncremental not yet wired"
  BuildDynamic{}      -> error "reflex-effectful: buildDynamic not yet wired"
  HeadE ev            -> sendReq (WOHeadE ev)
  Now                 -> sr @t WONow
  FoldDyn f z ev      -> sendReq (WOFoldDyn f z ev)
  Count ev            -> sendReq (WOCount ev)
  Toggle z ev         -> sendReq (WOToggle z ev)
  AccumDyn f z ev     -> sendReq (WOAccumDyn f z ev)
  AccumMaybeDyn f z ev -> sendReq (WOAccumMaybeDyn f z ev)
  FoldDynMaybe f z ev -> sendReq (WOFoldDynMaybe f z ev)

interpretPostBuild :: forall t es a. WidgetChannel t :> es => Eff (PostBuild t : es) a -> Eff es a
interpretPostBuild = interpret_ $ \case
  GetPostBuild -> sr @t WOGetPostBuild

interpretTriggerEvent :: forall t es a. WidgetChannel t :> es => Eff (TriggerEvent t : es) a -> Eff es a
interpretTriggerEvent = interpret_ $ \case
  NewTriggerEvent               -> sr @t WONewTriggerEvent
  NewTriggerEventWithOnComplete -> error "reflex-effectful: newTriggerEventWithOnComplete not yet wired"

interpretPerformEvent :: forall t es a. WidgetChannel t :> es => Eff (PerformEvent t : es) a -> Eff es a
interpretPerformEvent = interpret_ $ \case
  PerformEvent ev  -> sendReq (WOPerformEvent ev)
  PerformEvent_ ev -> sendReq (WOPerformEvent_ ev)

interpretDom :: forall t es a. (WidgetChannel t :> es, IOE :> es) => Eff (Dom t : es) a -> Eff es a
interpretDom = interpret $ \env -> \case
  Element_ tag cfg child -> do
    elRef <- liftIO $ newIORef Nothing
    sendReq (WOElementOpen tag cfg elRef)
    a <- localSeqUnlift env $ \unlift -> unlift child
    sendReq @t WOElementClose
    mel <- liftIO $ readIORef elRef
    case mel of
      Just el -> return (el, a)
      Nothing -> error "reflex-effectful: element handle not set after WOElementClose"
  Text_ t'     -> sr @t (WOText t')
  DynText_ d   -> sendReq (WODynText d)
  InputElement_ cfg    -> sendReq (WOInputElement cfg)
  TextAreaElement_ cfg -> sendReq (WOTextAreaElement cfg)
  SelectElement_ cfg child -> do
    selRef <- liftIO $ newIORef Nothing
    sendReq (WOSelectElementOpen cfg selRef)
    a <- localSeqUnlift env $ \unlift -> unlift child
    sendReq @t WOSelectElementClose
    msel <- liftIO $ readIORef selRef
    case msel of
      Just sel' -> return (sel', a)
      Nothing   -> error "reflex-effectful: selectElement handle not set"
  TextNode_ cfg     -> sendReq (WOTextNode cfg)
  CommentNode_ cfg  -> sendReq (WOCommentNode cfg)
  PlaceRawElement_{}  -> error "reflex-effectful: placeRawElement not yet wired"
  WrapRawElement_{}   -> error "reflex-effectful: wrapRawElement not yet wired"

interpretAdjustable :: forall t es a. (WidgetChannel t :> es, IOE :> es, Reflex.Reflex t) => Eff (Adjustable t : es) a -> Eff es a
interpretAdjustable = interpret $ \env -> \case
  RunWithReplace initial replaceEv -> do
    reg <- getRegistry @t
    localUnliftIO env (ConcUnlift Ephemeral Unlimited) $ \unlift -> do
      initialHandle <- startChild reg (unlift initial)
      let replaceActions = fmap (\eff -> startChild reg (unlift eff)) replaceEv
      sendReqIO reg (WORunWithReplace initialHandle replaceActions)
  TraverseIntMapWithKeyWithAdjust{} ->
    error "reflex-effectful: traverseIntMapWithKeyWithAdjust not yet wired"
  TraverseDMapWithKeyWithAdjust{} ->
    error "reflex-effectful: traverseDMapWithKeyWithAdjust not yet wired"
  TraverseDMapWithKeyWithAdjustWithMove{} ->
    error "reflex-effectful: traverseDMapWithKeyWithAdjustWithMove not yet wired"

interpretPrerender :: forall t es a. (WidgetChannel t :> es, IOE :> es, Reflex.Reflex t) => Eff (Prerender t : es) a -> Eff es a
interpretPrerender = interpret $ \env -> \case
  Prerender _server client -> do
    a <- localSeqUnlift env $ \unlift -> unlift client
    sendReq (WOHoldDyn a Reflex.never)

interpretJSM :: forall t es a. WidgetChannel t :> es => Eff (JSM' : es) a -> Eff es a
interpretJSM = interpret_ $ \case
  LiftJSM jsm -> sr @t (WOLiftJSM jsm)

interpretHasDocument :: forall t es a. WidgetChannel t :> es => Eff (HasDocument : es) a -> Eff es a
interpretHasDocument = interpret_ $ \case
  AskDocument -> sr @t WOAskDocument

interpretDomRenderHook :: forall t es a. WidgetChannel t :> es => Eff (DomRenderHook t : es) a -> Eff es a
interpretDomRenderHook = interpret $ \env -> \case
  WithRenderHook _hook child -> localSeqUnlift env $ \unlift -> unlift child
  RequestDomAction ev  -> sendReq (WORequestDomAction ev)
  RequestDomAction_ ev -> sendReq (WORequestDomAction_ ev)

interpretNotReady :: forall t es a. WidgetChannel t :> es => Eff (NotReady t : es) a -> Eff es a
interpretNotReady = interpret_ $ \case
  NotReadyUntil ev -> sendReq (WONotReadyUntil ev)
  NotReady_        -> sr @t WONotReady

interpretEventWriter :: forall t w es a. WidgetChannel t :> es => Eff (EventWriter t w : es) a -> Eff es a
interpretEventWriter = interpret_ $ \(TellEvent ev) -> sendReq (WOTellEvent ev)

interpretDynamicWriter :: forall t w es a. WidgetChannel t :> es => Eff (DynamicWriter t w : es) a -> Eff es a
interpretDynamicWriter = interpret_ $ \(TellDyn d) -> sendReq (WOTellDyn d)

interpretBehaviorWriter :: forall t w es a. WidgetChannel t :> es => Eff (BehaviorWriter t w : es) a -> Eff es a
interpretBehaviorWriter = interpret_ $ \(TellBehavior b) -> sendReq (WOTellBehavior b)

interpretAll
  :: forall t es a. (WidgetChannel t :> es, IOE :> es, Reflex.Reflex t)
  => Eff ( Dom t : Hold t : Sample t : PostBuild t
         : PerformEvent t : TriggerEvent t
         : Adjustable t : Prerender t
         : JSM' : HasDocument : DomRenderHook t
         : NotReady t : es) a
  -> Eff es a
interpretAll
  = interpretNotReady @t
  . interpretDomRenderHook @t
  . interpretHasDocument @t
  . interpretJSM @t
  . interpretPrerender @t
  . interpretAdjustable @t
  . interpretTriggerEvent @t
  . interpretPerformEvent @t
  . interpretPostBuild @t
  . interpretSample @t
  . interpretHold @t
  . interpretDom @t
