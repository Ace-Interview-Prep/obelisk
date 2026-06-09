{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Reflex.Text
--  Copyright   :  (c) 2024, Galen Sprout
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Galen Sprout <galen.sprout@gmail.com>
--  Stability   :  provisional
--  Portability :  portable
--
--  A collection of Classh-typed versions of elClass and similar funcs using classhUnsafe
--
-------------------------------------------------------------------------------


module Classh.Reflex.El where

import Reflex.Dom.Core
import Classh
import Control.Monad (void)
import qualified Data.Map as Map
import qualified Data.Text as T




type Shell m a = m a -> m a

-- | is elClass except that it takes a BoxConfig instead of a string for classes
elTW :: DomBuilder t m => T.Text -> BoxConfig -> m a -> m a
elTW etag cfg ma = elClass etag (defaultClasses <> " " <> showTW cfg) ma

-- | is elClass' except that it takes a BoxConfig instead of a string for classes.
elTW' :: DomBuilder t m => T.Text -> BoxConfig -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
elTW' etag cfg ma = elClass' etag (defaultClasses <> " " <> showTW cfg) ma

-- | is elDynClass except that it takes a BoxConfig instead of a string for classes
elDynTW :: (PostBuild t m, DomBuilder t m) => T.Text -> Dynamic t BoxConfig -> m a -> m a
elDynTW etag cfgDyn ma = elDynClass etag ( (\cfg -> defaultClasses <> " " <> showTW cfg) <$> cfgDyn ) ma

-- | is elDynClass' except that it takes a BoxConfig instead of a string for classes
elDynTW' :: (PostBuild t m, DomBuilder t m) => T.Text -> Dynamic t BoxConfig -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
elDynTW' etag cfgDyn ma = elDynClass' etag ( (\cfg -> defaultClasses <> " " <> showTW cfg) <$> cfgDyn ) ma

div_ :: DomBuilder t m => m a -> m a
div_ = el "div"

-- | When we want to do absolutely nothing but create an area devoid of influence by outside
-- | Most common case is for differing displays like inline-block vs grid
-- |
-- | In practice this can also be used to solve a common problem where
-- | helper functions unnecessarily restrict size of children.
-- |
-- | TODO: we need to probably use this everywhere
in_ :: DomBuilder t m => m a -> m a
in_ = divClass $(classh' [h .~~ pct 100, w .~~ pct 100])

-- | Example Use Case:
-- | divClass $(classh' [pos .~~ centered]) $ do
-- |   inH_ leaf
inH_ :: DomBuilder t m => m a -> m a
inH_ = divClass $(classh' [h .~~ pct 100])


type PlaceHolder = T.Text
inputEl :: DomBuilder t m => T.Text -> T.Text -> PlaceHolder -> m ()
inputEl elClasses textClasses placeholder = void $ inputElement $ def & inputElementConfig_elementConfig . initialAttributes .~
  ("class" =: (elClasses <&> textClasses)
  <> "placeholder" =: placeholder
  )

type TextAreaRows = Int
textAreaEl :: DomBuilder t m => TextAreaRows -> T.Text -> T.Text -> PlaceHolder -> m ()
textAreaEl rows elClasses textClasses placeholder =
  void $ textAreaElement $ def & textAreaElementConfig_elementConfig . initialAttributes .~
  ("class" =: (elClasses <&> textClasses)
  <> "rows" =: tshow rows
  <> "placeholder" =: placeholder
  )

-- <img
--   src="dog-600.jpg"
--   srcset="
--     dog-300.jpg 300w,
--     dog-450.jpg 450w,
--     dog-600.jpg 600w,
--     dog-900.jpg 900w,
--     dog-1200.jpg 1200w,
--     dog-1600.jpg 1600w
--   "
--   sizes="(max-width: 600px) 100vw, (max-width: 900px) 50vw, 33vw"
--   alt="Dog running through the field" />
type ScreenWidth = DimensionConstraint
imgResponsive
  :: DomBuilder t m
  => [ (StaticImagePath
       , (CSSSize, ScreenWidth)
       )
     ]
  -> CompiledS
  -> m ()
imgResponsive imageInfo classes = elAttr "img" (imgSrcSet imageInfo classes) blank

-- example = imgResponsive (imgSrcSet [] "")

--imgSrcSet
type StaticImagePath = T.Text
type Pixels = Int

imgSrcSet :: [(StaticImagePath, (CSSSize, DimensionConstraint))] -> CompiledS -> Map.Map T.Text T.Text
imgSrcSet [] _ = error "No images given to Classh.Reflex.imgSrcSet" -- we know this must be the start, there is no recursion
imgSrcSet xs classes =
  let
    mkPair = \(src',(w_,_)) -> src' <&> (renderCSS w_) <> "w"
    pairs = mkPair <$> xs

    showMW :: DimensionConstraint -> T.Text
    showMW m_ = "(max-width: " <> showTW m_ <> ")"

    mkFuckingAlgebraicSizes :: [(StaticImagePath, (CSSSize, DimensionConstraint))] -> T.Text
    mkFuckingAlgebraicSizes [] = error "mkFuckingAlgebraicSizes received no input"
    mkFuckingAlgebraicSizes ((_,(w_, maxW__)):xs_) =
      if null xs_
      then renderCSS w_
      else showMW maxW__ <> renderCSS w_ <> "," <> mkFuckingAlgebraicSizes xs_
  in
    "srcset" =: (T.intercalate "," pairs)
    <> "sizes" =: (mkFuckingAlgebraicSizes xs)
    <> "class" =: classes

divClassh :: BoxConfig -> [BoxConfig -> BoxConfig] -> Compiled Expression
divClassh base muts = case compileS $ foldl (\acc f -> f acc) base muts of
  Left e -> fail $ T.unpack e
  Right styleString -> [| divClass styleString |]

divClassh' :: [BoxConfig -> BoxConfig] -> Compiled Expression
divClassh' muts = case compileS $ foldl (\acc f -> f acc) def muts of
  Left e -> fail $ T.unpack e
  Right styleString -> [| divClass styleString |]

textClassh :: TextConfigTW -> [TextConfigTW -> TextConfigTW] -> Compiled Expression
textClassh base muts = case compileS $ foldl (\acc f -> f acc) base muts of
  Left e -> fail $ T.unpack e
  Right styleString -> [| textS styleString |]

textClassh' :: [TextConfigTW -> TextConfigTW] -> Compiled Expression
textClassh' muts = case compileS $ foldl (\acc f -> f acc) def muts of
  Left e -> fail $ T.unpack e
  Right styleString -> [| textS styleString |]

textPos' :: [TextPosition -> TextPosition] -> Compiled Expression
textPos' muts = case compileS $ foldl (\acc f -> f acc) def muts of
  Left e -> fail $ T.unpack e
  Right styleString -> [| textPosition styleString |]

textPos :: TextPosition -> [TextPosition -> TextPosition] -> Compiled Expression
textPos base muts = case compileS $ foldl (\acc f -> f acc) base muts of
  Left e -> fail $ T.unpack e
  Right styleString -> [| textPosition styleString |]
