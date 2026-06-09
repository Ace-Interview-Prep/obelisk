{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

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
--  A collection of interfaces to handle text from a single word to multiple paragraphs 
--
-------------------------------------------------------------------------------


module Classh.Reflex.Text where

import Classh.Reflex.Layout (row)
import Reflex.Dom.Core
import Classh

import qualified Data.Text as T


-- TODO TEXT CONTAINER WITH STATIC SIZING 

--  TODO: variants which do not get checked/compiled

-- | Text wrapped in a span to ensure classes work properly regardless of context 
textS :: DomBuilder t m => CompiledS -> T.Text -> m ()
textS s txt = elClass "span" s $ text txt

-- | Text wrapped in a \'u\' tag  to ensure classes work properly regardless of context 
textU :: DomBuilder t m => CompiledS -> T.Text -> m ()
textU s txt = elClass "u" s $ text txt

-- | Like textS except that the classes are Dynamic so they may change in response to user events
textDynS
  :: ( PostBuild t m
     , DomBuilder t m
     )
  => Dynamic t CompiledS
  -> T.Text
  -> m ()
textDynS s txt = elDynClass "span" s $ text txt

-- | Like textS except that the Text is Dynamic so they may change in response to user events
-- | This is just the classhified version of dynText 
dynTextS
  :: ( PostBuild t m
     , DomBuilder t m
     )
  => CompiledS
  -> Dynamic t T.Text
  -> m ()
dynTextS s txt = elClass "span" s $ dynText txt

-- | Like textS except that the Text and class attribute is Dynamic so they may change in response to user events
dynTextDynS
  :: ( PostBuild t m
     , DomBuilder t m
     )
  => Dynamic t CompiledS
  -> Dynamic t T.Text
  -> m ()
dynTextDynS s txt = elDynClass "span" s $ dynText txt

-- | For creating generic Templates which dont deeply depend on the text or text config given, or allow
-- | for multiple 'TextConfigTW's across a single text.
-- | A simple example is a sentence with highlighted or different color words for emphasis
-- | The one caveat is you do need to manually set the TextSize.
--  > intercalate (only XL2) " " [ textS $(classh' [text_size .~~ XL2, text_color .~~ Black]) "Hey"
--  >                            , dynTextS $(classh' [text_size .~~ XL2, text_color .~~ White]) (user :: Dynamic t String)
--  >                            ]
intercalate
  :: DomBuilder t m
  => WhenTW TextSize
  -> T.Text
  -> [m ()]
  -> m ()
intercalate _ _ [] = pure () 
intercalate textSize inter (txt':txts') = do
  txt' >> prependAll textSize inter txts'
  where
    prependAll _ _ [] = pure ()
    prependAll s i (txt:txts) = elClass "span" (renderWhenTW s showTW ) (text i) >> txt >> prependAll s i txts

-- | Works by setting top padding by one consistent value. Does not add padding for first element

-- | Display a number of paragraphs (where a paragraph is 1 or more sentences, with whatever crazy styling, like intercalate)
-- | with even spacing between them, defined by the first arg of TWSize 
paragraphs
  :: DomBuilder t m
  => WhenTW TWSize
  -> [m ()]
  -> m ()
paragraphs _ [] = pure ()
paragraphs spacing (para:paras) = do
  row [] $ para
  withTopPadding spacing paras
  where
    withTopPadding _ [] = pure ()
    withTopPadding p_ (r_:rs_) = row [t .~ fmap (\(c, v) -> (c, noTransition v)) p_] r_ >> withTopPadding p_ rs_

-- | Display a number of paragraphs (where a paragraph is 1 or more sentences, with whatever crazy styling, like intercalate)
-- | with variable spacing between them, defined by the first element of the tuple, of type TWSize
paragraphs'
  :: DomBuilder t m
  => [(WhenTW TWSize, m ())]
  -> m ()
paragraphs' [] = pure ()
paragraphs' ((topPadding,para):rows) = row [t .~ fmap (\(c, v) -> (c, noTransition v)) topPadding] para >> paragraphs' rows



textPosition :: DomBuilder t m => CompiledS -> m a -> m a
textPosition = divClass
