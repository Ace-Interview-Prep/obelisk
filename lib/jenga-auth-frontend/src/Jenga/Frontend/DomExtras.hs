{-# LANGUAGE OverloadedStrings #-}

module Jenga.Frontend.DomExtras where

import Control.Lens ((%~))
import Jenga.Route.Frontend (RouteToUrl, SetRoute, askRouteToUrl)
import Reflex (Reflex, Event, Dynamic, current, tag, updated, leftmost)
import Reflex.Dom.Core (GhcjsDomSpace, Element, EventResult, AttributeName(..)
                       , elementConfig_initialAttributes, elementConfig_eventSpec
                       , addEventSpecFlags, Click(..), preventDefault, domEvent
                       , InputElementConfig, InputElement)
import qualified GHCJS.DOM.Element as GElement
import Control.Monad (when)
import Data.Default (def)
import Data.Proxy (Proxy(..))
import qualified Data.Map as Map
import qualified Data.Text as T

import Effectful (Eff, (:>))
import Reflex.Effectful.Effect.Dom (Dom, element, blank)
import Reflex.Effectful.Effect.PostBuild (PostBuild, getPostBuild)
import Reflex.Effectful.Effect.PerformEvent (PerformEvent, performEvent_)
import Reflex.Effectful.Effect.JSM (JSM', liftJSM)

elDynHtmlAttr_
  :: ( Dom t :> es
     , PostBuild t :> es
     , PerformEvent t :> es
     , JSM' :> es
     , Reflex t
     )
  => T.Text
  -> Map.Map T.Text T.Text
  -> Dynamic t T.Text
  -> Eff es (Element EventResult GhcjsDomSpace t)
elDynHtmlAttr_ elementTag attrs_ html = do
  let cfg = def & initialAttributes .~ Map.mapKeys (AttributeName Nothing) attrs_
  (e, _) <- element elementTag cfg $ pure ()
  postBuild <- getPostBuild
  performEvent_ $ ffor (leftmost [updated html, tag (current html) postBuild]) $ \h ->
    liftJSM $ GElement.setInnerHTML (_element_raw e) h
  return e
  where
    initialAttributes = undefined -- TODO: need lens from reflex-dom
    _element_raw = undefined -- TODO: need accessor
    ffor ev f = f <$> ev

routeLinkImpl
  :: forall t es a route.
     ( Dom t :> es
     , RouteToUrl route :> es
     , SetRoute t route :> es
     , Reflex t
     )
  => Map.Map AttributeName T.Text
  -> route
  -> Eff es a
  -> Eff es (Event t (), a)
routeLinkImpl attrs route wrappedChild = do
  enc <- askRouteToUrl
  let
    targetBlank = Map.lookup "target" attrs == Just "_blank"
    cfg = (def :: ElementConfig EventResult t GhcjsDomSpace)
        & elementConfig_initialAttributes .~ ("href" =: enc route <> attrs)
        & (if targetBlank
           then id
           else elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy GhcjsDomSpace) Click (const preventDefault))
  (e, a) <- element "a" cfg wrappedChild
  when targetBlank $ pure ()
  return (domEvent Click e, a)
  where
    ElementConfig = undefined -- TODO: needs reflex-dom types in scope
