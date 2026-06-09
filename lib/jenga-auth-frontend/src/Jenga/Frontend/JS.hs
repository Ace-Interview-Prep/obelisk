{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Jenga.Frontend.JS where

import Jenga.Common.HasJengaConfig
import Jenga.Route (HasRoute, renderRoute)
import Reflex (Reflex, Event, Dynamic, never, ffor)
import Reflex.Dom.Core (GhcjsDomSpace, Element, EventResult, postJson, newXMLHttpRequestWithError
                       , XhrException, XhrResponse, _xhrResponse_responseText)
import Language.Javascript.JSaddle
import qualified GHCJS.DOM.Types as GHCJS
import Control.Lens ((^.))
import Control.Monad (void)
import Data.Proxy (Proxy)
import qualified Data.Text as T

import Effectful (Eff, (:>), IOE)
import Effectful.Reader.Static (Reader, ask)
import Reflex.Effectful.Effect.Hold (Hold, holdDyn, foldDyn, headE)
import Reflex.Effectful.Effect.PostBuild (PostBuild, getPostBuild)
import Reflex.Effectful.Effect.TriggerEvent (TriggerEvent, newTriggerEvent)
import Reflex.Effectful.Effect.PerformEvent (PerformEvent, performEvent)
import Reflex.Effectful.Effect.Dom (Dom, el, text, dynText, blank)
import Reflex.Effectful.Effect.Prerender (Prerender, prerender, prerender_)
import Reflex.Effectful.Effect.JSM (JSM', liftJSM)

runJS :: (JSM' :> es, Prerender t :> es, Reflex t)
      => JSM a
      -> Eff es (Dynamic t (Either String a))
runJS jsStatement = prerender (pure $ Left "Couldn't get JS value from its context") (Right <$> liftJSM jsStatement)

runJSWhen :: (Prerender t :> es, PerformEvent t :> es, Reflex t)
           => Event t a
           -> (a -> JSM b)
           -> Eff es (Event t b)
runJSWhen event jsFunc = do
  d <- prerender (pure never) $ performEvent $ ffor event $ \a -> liftJSM (jsFunc a)
  -- switchDyn equivalent: hold the inner event
  holdDyn never d >>= pure . switchDyn
  where switchDyn = undefined -- TODO: needs switchDyn as effect

runJSWhenPb :: ( Prerender t :> es, PostBuild t :> es, PerformEvent t :> es
               , TriggerEvent t :> es, Hold t :> es, IOE :> es, Reflex t)
            => JSM b -> Eff es (Event t b)
runJSWhenPb jsStatement = do
  pBuild <- getPostBuild
  output <- runJSWhen pBuild $ \_ -> jsStatement
  headE output

runJSWhenRendered :: ( Prerender t :> es, PostBuild t :> es, PerformEvent t :> es
                     , TriggerEvent t :> es, Hold t :> es, IOE :> es, Reflex t)
  => JSM b -> Eff es (Event t b)
runJSWhenRendered jsStatement = do
  pBuild <- onRender
  output <- runJSWhen pBuild $ \_ -> jsStatement
  headE output

onRender :: (Prerender t :> es, Dom t :> es, Reflex t) => Eff es (Event t ())
onRender = do
  d <- prerender blank blank
  pure $ updated d
  where updated = undefined -- TODO: needs `updated` from Dynamic

clog :: (JSM' :> es, ToJSVal a) => a -> Eff es ()
clog a = liftJSM $ do
  _ <- jsg "console" ^. js1 "log" a
  pure ()

clogErr :: forall e es a. (ToJSVal e, JSM' :> es) => e -> Eff es (Either e a)
clogErr e = liftJSM $ do
  _ <- jsg "console" ^. js1 "error" e
  pure $ Left e

clogSend
  :: forall api r es a.
     ( JSM' :> es
     , ToJSVal a
     , HasRoute api r
     , Reader cfg :> es, HasConfig cfg BaseURL
     )
  => Proxy api
  -> r
  -> a
  -> Eff es ()
clogSend proxy route a = do
  clog a
  logRoute <- renderFullRouteBE proxy route
  liftJSM $ do
    strA <- valToStr a
    let req = postJson (getLink logRoute) (show strA)
    _ <- newXMLHttpRequestWithError req $ \err ->
      let
        showXhr = \case
          Left e -> show (Left e :: Either XhrException String)
          Right res -> show (Right $ _xhrResponse_responseText res :: Either XhrException (Maybe T.Text))
      in
        clog $ showXhr err
    pure ()

type HTMLRef = JSVal
type HTMLEventType = T.Text
type HTMLEvent = JSVal

emptyEvent :: HTMLEvent -> JSM ()
emptyEvent = pure . const ()

addHTMLEventListener
  :: ( TriggerEvent t :> es, Prerender t :> es, JSM' :> es, Dom t :> es, Reflex t )
  => HTMLRef -> HTMLEventType -> (HTMLEvent -> JSM a) -> Eff es (Event t a)
addHTMLEventListener htmlRef htmlEventType actn = do
  (event, trigger) <- newTriggerEvent
  prerender_ blank $ liftJSM $ void $ htmlRef ^. js2 "addEventListener" htmlEventType
    (fun $ \_ _ e -> do
        out <- actn $ head e
        liftIO $ trigger out
    )
  pure event

currentWindowUnchecked :: MonadJSM m => m JSVal
currentWindowUnchecked = liftJSM $ jsg "window"

currentDocumentUnchecked :: MonadJSM m => m GHCJS.Document
currentDocumentUnchecked = liftJSM $ fmap GHCJS.Document $ jsg "document"

traceDyn' :: (Trace t es, Show a) => Dynamic t a -> Eff es ()
traceDyn' d = do
  let ev = updated d
  traceEvent' (T.pack "traceDyn") ev
  where updated = undefined -- TODO

type Trace t es = (Dom t :> es, Hold t :> es, PostBuild t :> es, Reflex t)

traceEvent' :: (Trace t es, Show a) => T.Text -> Event t a -> Eff es ()
traceEvent' label ev = do
  el (T.pack "div") $ do
    el (T.pack "div") $ text label
    vals <- foldDyn (\a_ b_ -> a_ : b_) [] ev
    el (T.pack "div") $ dynText $ T.pack . show <$> vals
