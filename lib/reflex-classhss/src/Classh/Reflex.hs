{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Reflex.Layout
--  Copyright   :  (c) 2024, Galen Sprout
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Galen Sprout <galen.sprout@gmail.com>
--  Stability   :  provisional
--  Portability :  portable
--
--  A collection of patterns that have emerged from usage of ClasshSS in order to reduce the amount of
--  code necessary to build beautiful UIs that will be easier to maintain.
--
--  For example, lets use grids to show 3 components (just text here) that on mobile and
--  tablet(sm) will take up the full row, and at larger sizes take up 3\/12 width, 4\/12 width, and 5\/12 width
--  respectively.
--
--  Let's also pretend this is a landing page and we want all rows evenly spaced, we can make this obvious
--  with 'row'
-- 
-- @
--   {-# LANGUAGE OverloadedStrings #-}
--   {-# LANGUAGE FlexibleContexts #-}
--   module Main where
--
--   import Reflex.Dom.Core
--   import Classh
--   
--   mySimpleResponsivePage :: DomBuilder t m => m ()
--   mySimpleResponsivePage = do 
--     row [y .~~ TWSize 10] $ do 
--       gridCol Col12 $ do
--         col [12,12,3] $ normalText "hey"
--         col [12,12,4] $ normalText "hello"
--         col [12,12,5] $ normalText "howdy"
--     where
--        normalText = textS $(classh' [text_size .|~ [LG, XL, XL2, XL3] ] )
-- @
--
-- 
-------------------------------------------------------------------------------


module Classh.Reflex ( module X) where

import Classh.Reflex.El as X
import Classh.Reflex.Text as X
import Classh.Reflex.Place as X
import Classh.Reflex.Layout as X




-- | Not real: just ideating
-- | How can we model a discrete set of consistent brand options?
--
-- data Mytextsize = Heading | Subtext | Normal
-- domain = Map.fromList [ (Heading , XL7) , (Normal , XL3) , (Subtext , XS) ]
--
-- brandF :: k -> WhenTW k
-- brandF (domainType :: Ord k => k) =
--   let baseSize = fromJust $ Map.lookup domainType (domain :: Map (k :: Ord k => k) (v :: ShowTW x => x))
--   in
--     autoScale baseSize
--
-- then this above code suggests that we should think of our domain as mobile first
-- it is important to remember this is not meant to represent 100% of CSS but rather ~80%
-- and should be a way to communicate branding concisely such that a stoopid cud impl. it to be
-- consistent
--
-- Now, it wont always be super complex: for example, color
--
-- def = TW.def & text_color .~~ aceBlue
--
-- But then again, a brand may have multiple colors
--
-- brandColors = Map.fromList [ (AceBlue, hex _), (AceNavyBlue, hex _), (AceBabyBlue, hex _) ]
-- and also:
       -- What about dark mode? \
       --
-- and note: lets say 90% of titles should have Bold as their weight, we can set this and








-- | TODO: styledParagraphs :: [( BottomPadding, [(tw,T.Text)])] -> m ()
-- then apply to bannerFor in Landing.*


-- buttonC :: Map.Map T.Text T.Text -> m a -> m (Event t a)
-- buttonC attrs inner = do
--   (e,x) <- elAttr' "button" attrs inner
--   pure $ x <$ domEvent Click e



-- textTW' :: (DomBuilder t m, ShowTW tw) => tw -> T.Text -> m ()
-- textTW' = styledText'

-- styledText' :: (DomBuilder t m, ShowTW tw) => tw -> T.Text -> m ()
-- styledText' cfg txt = elClass "span" (showTW cfg) $ text txt

-- dynTextTW' :: (DomBuilder t m, PostBuild t m, ShowTW tw) => tw -> Dynamic t T.Text -> m ()
-- dynTextTW' cfg txt = elClass "span" (showTW cfg) $ dynText txt

-- textDynTW' :: (DomBuilder t m, PostBuild t m, ShowTW tw) => Dynamic t tw -> T.Text -> m ()
-- textDynTW' dynCfg txt = elDynClass "span" ( (\cfg -> defaultClasses <> " " <> showTW cfg) <$> dynCfg ) $ text txt

-- styledTexts' :: (ShowTW tw, DomBuilder t m) => [(tw, T.Text)] -> m ()
-- styledTexts' styTexs = el "div" $ forM_ styTexs $ \(cfg, txt) -> styledText' cfg txt


