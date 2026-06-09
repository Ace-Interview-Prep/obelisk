{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Reflex.Place
--  Copyright   :  (c) 2024, Galen Sprout
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Galen Sprout <galen.sprout@gmail.com>
--  Stability   :  provisional
--  Portability :  portable
--
--  A collection of ways to quickly place an element within a parent
--
-------------------------------------------------------------------------------



module Classh.Reflex.Place where

--import Classh.Reflex.Layout
import Reflex.Dom.Core
import Classh

-- | A row of some target width to place a component/container
placeCenterWidth :: DomBuilder t m => WhenTW TWSizeOrFraction -> m a -> m a
placeCenterWidth widthResponsive = centerSimple . divClass (classhUnsafe [ w .~ fmap (\(c, v) -> (c, noTransition v)) widthResponsive ])

-- | A collection of classes to center horizontally and vertically
centerXY :: ClassCollection BoxConfig
centerXY = ClassCollection [ position .~~ centered, w .~~ pct 100, h .~~ pct 100 ]

-- | A collection of classes to center horizontally
centerX :: ClassCollection BoxConfig
centerX = ClassCollection [ position .~~ centered, w .~~ pct 100 ]

-- | A collection of classes to center vertically
centerY :: ClassCollection BoxConfig
centerY = ClassCollection [ position .~~ centered, h .~~ pct 100 ]

-- | A utility to easily place the element given as an arg in the center (vertically,horizontally) of its parent
centerSimple :: DomBuilder t m => m a -> m a
centerSimple = divClass $(classh' [ position .~~ centered, w .~~ pct 100, h .~~ pct 100 ])

-- | A utility to easily place the element given as an arg in the center (horizontally) of its parent
centerHSimple :: DomBuilder t m => m a -> m a
centerHSimple = divClass $(classh' [ position .~~ centered, w .~~ pct 100 ])

-- | A utility to easily place the element given as an arg in the center (vertically) of its parent
centerVSimple :: DomBuilder t m => m a -> m a
centerVSimple = divClass $(classh' [ position .~~ centered, h .~~ pct 100 ])

-- | A utility to easily place the element-arg centered between some padding 
responsiveXPaddedRegion :: DomBuilder t m => [TWSizeOrFraction] -> m a -> m a
responsiveXPaddedRegion opts = centerHSimple . divClass (classhUnsafe [w .|~ opts])

-- | A utility to easily place the element-arg centered between some padding, with pre-chosen values
-- | this is really useful for prototyping
responsiveXPaddedRegion' :: DomBuilder t m => m a -> m a
responsiveXPaddedRegion' = responsiveXPaddedRegion [pct 100, pct 80, pct 80, pct 80, pct 80, pct 70]


  
