module Jenga.Frontend.Scroll where

import Jenga.Frontend.JS
import Jenga.Frontend.DomExtras
import Jenga.Route.Frontend (RouteToUrl, SetRoute)
import Reflex (Reflex, Event, ffor)
import Language.Javascript.JSaddle
import qualified GHCJS.DOM.Types as DOM
import Control.Lens ((^.))
import Control.Monad (void)
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Data.Text as T

import Effectful (Eff, (:>))
import Reflex.Effectful.Effect.Dom (Dom, blank)
import Reflex.Effectful.Effect.PostBuild (PostBuild)
import Reflex.Effectful.Effect.PerformEvent (PerformEvent, performEvent_)
import Reflex.Effectful.Effect.Prerender (Prerender, prerender_)
import Reflex.Effectful.Effect.JSM (JSM', liftJSM)

routeLinkHash
  :: forall t es a route.
     ( Dom t :> es
     , RouteToUrl route :> es
     , SetRoute t route :> es
     , Prerender t :> es
     , PostBuild t :> es
     , Reflex t
     )
  => route
  -> Maybe T.Text
  -> T.Text
  -> Eff es a
  -> Eff es a
routeLinkHash route mFragContainer frag wrappedChild = do
  (_e, a) <- routeLinkImpl mempty route wrappedChild
  scrollToFrag mFragContainer frag _e
  return a

scrollToTop :: (JSM' :> es) => Eff es ()
scrollToTop = liftJSM $ void $ jsg ("window") ^. js2 ("scrollTo") (0 :: Int) (0 :: Int)

scrollElem :: (JSM' :> es) => JSVal -> (Float,Float) -> Eff es ()
scrollElem e (x_,y_) = liftJSM $ void $ e ^. js1 ("scrollBy") opts
  where opts = do
          o <- create
          o ^. jss ("top") y_
          o ^. jss ("left") x_
          o ^. jss ("behavior") ("smooth")
          pure o

getDimensions :: (JSM' :> es) => T.Text -> Eff es (Float, Float)
getDimensions idTag = liftJSM $ do
  doc <- jsg ("document")
  idEl <- doc ^. js1 ("getElementById") idTag
  width_ <- fromJSVal =<< idEl ^. js ("clientWidth")
  height_ <- fromJSVal =<< idEl ^. js ("clientHeight")
  clog (width_,height_)
  pure (fromMaybe 500 width_, fromMaybe 500 height_)

scrollToId :: (JSM' :> es) => Maybe T.Text -> T.Text -> Eff es ()
scrollToId containerId idFrag = liftJSM $ do
  w' <- Just <$> currentWindowUnchecked
  doc' <- DOM.unDocument <$> currentDocumentUnchecked
  containerRef <- case containerId of
    Nothing -> pure Nothing
    Just idC -> do
      cont <- doc' ^. js1 ("getElementById") idC
      ghcjsPure (isNull cont) >>= \case
        True -> pure Nothing
        False -> pure $ Just cont
  clog (doc', containerRef)
  case (,,) <$> (containerRef <|> (Just doc')) <*> Just doc' <*> w' of
    Nothing -> pure ()
    Just (container, doc, window) -> do
      eWithId <- doc ^. js1 ("getElementById") idFrag
      clog =<< eWithId ^. js0 ("getBoundingClientRect")
      targetY :: Maybe Int <- fromJSVal =<< (eWithId ^. js0 ("getBoundingClientRect") ^. js ("top"))
      containerY :: Maybe Int <- fromJSVal =<< (container ^. js0 ("getBoundingClientRect") ^. js ("top"))
      scrollY' :: Maybe Int <- fromJSVal =<< window ^. js ("scrollY")
      containerScrollTop :: Maybe Int <- fromJSVal =<< container ^. js ("scrollTop")
      clog ("scroll top", containerScrollTop)
      case (,,,) <$> targetY <*> containerY <*> containerScrollTop <*> scrollY' of
        Nothing -> pure ()
        Just (tTop, cTop, _cScrollTop, scrollY) -> do
          clog (tTop, cTop, _cScrollTop)
          opts <- do
            o <- create
            o ^. jss ("top") (tTop - (cTop + scrollY))
            o ^. jss ("behavior") ("smooth")
            pure o
          _ <- container ^. js1 ("scrollTo") opts
          clog opts
          clog container
          pure ()

scrollToFrag :: forall es t. (Prerender t :> es, PerformEvent t :> es, Dom t :> es, JSM' :> es, Reflex t)
  => Maybe T.Text -> T.Text -> Event t () -> Eff es ()
scrollToFrag mContainer fragId e = prerender_ blank $ performEvent_ $ ffor e $ \_ -> scrollToId mContainer fragId
  where ffor ev f = f <$> ev
