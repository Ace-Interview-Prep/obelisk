{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

-- |
-- Example DOM using the inline-block layout system.
--
-- Children own their widths. The type-level budget system
-- prevents overflow at compile time. No flexbox. No grid-template-columns.
--
-- To run: this needs a full obelisk/reflex build environment.

module InlineBlockExample where

import Reflex.Dom.Core
import Classh
import Classh.Reflex.InlineBlock
import Control.Monad.Tree.Grid.Indexed (N, F, (.>>), runChildren)
import qualified Control.Monad.Tree.Grid.Indexed as G


--------------------------------------------------------------------------------
-- Example 1: Simple 30/70 split
--------------------------------------------------------------------------------

-- Two panels side by side. 30% sidebar, 70% main content.
-- Budget: 30 + 70 = 100 ✓
simpleSplit :: DomBuilder t m => m ()
simpleSplit = ibRow $
  ib @(F 30 100) sidebar .>> ib @(F 70 100) mainContent
  where
    sidebar     = elClass "div" $(classh' [bgColor .~~ solidColor (Gray C100), p .~~ TWSize 4]) $
                    text "Sidebar (30%)"
    mainContent = elClass "div" $(classh' [bgColor .~~ solidColor White, p .~~ TWSize 4]) $
                    text "Main Content (70%)"


--------------------------------------------------------------------------------
-- Example 2: Thirds
--------------------------------------------------------------------------------

-- Three equal columns using Tailwind-native fractions.
-- Budget: 1/3 + 1/3 + 1/3 = 1 ✓
--
-- Renders as: w-1/3 (not w-[33%])
thirds :: DomBuilder t m => m ()
thirds = ibRowExact $
  ib @(F 1 3) card1 .>> ib @(F 1 3) card2 .>> ib @(F 1 3) card3
  where
    card n = elClass "div" $(classh' [p .~~ TWSize 4, br .~~ R_Lg, shadow .~~ Shadow_Md]) $
               text n
    card1 = card "Card 1"
    card2 = card "Card 2"
    card3 = card "Card 3"


--------------------------------------------------------------------------------
-- Example 3: Asymmetric layout
--------------------------------------------------------------------------------

-- 22% + 5% + 30% = 57%. Leftover is fine (ibRow allows it).
-- The 22% has no Tailwind fraction — renders as w-[22%].
-- The 5% renders as w-[5%].
-- The 30% renders as w-[30%].
asymmetric :: DomBuilder t m => m ()
asymmetric = ibRow $
  ib @(F 22 100) panelA
    .>> ib @(F 5 100) spacer
    .>> ib @(F 30 100) panelB
  where
    panelA = elClass "div" $(classh' [bgColor .~~ solidColor (Blue C100)]) $ text "22%"
    spacer = el "div" blank
    panelB = elClass "div" $(classh' [bgColor .~~ solidColor (Blue C200)]) $ text "30%"


--------------------------------------------------------------------------------
-- Example 4: Multi-row with QualifiedDo
--------------------------------------------------------------------------------

-- Two rows in a 100-unit budget.
-- Row 1: full-width header (100/100)
-- Row 2: sidebar (25/100) + main (75/100)
--
-- QualifiedDo's >>= resets budget to total between rows.
multiRow :: DomBuilder t m => m ()
multiRow = ibRow $ G.do
  ib @(F 100 100) header
  ib @(F 25 100) sidebar .>> ib @(F 75 100) mainArea
  where
    header   = elClass "div" $(classh' [bgColor .~~ solidColor (Indigo C600), p .~~ TWSize 4]) $
                 textS $(classh' [text_color .~~ color White, text_weight .~~ Bold]) "Header"
    sidebar  = elClass "div" $(classh' [bgColor .~~ solidColor (Gray C100), p .~~ TWSize 4]) $
                 text "Nav"
    mainArea = elClass "div" $(classh' [bgColor .~~ solidColor White, p .~~ TWSize 4]) $
                 text "Main"


--------------------------------------------------------------------------------
-- Example 5: Nested layout
--------------------------------------------------------------------------------

-- Top level: 50/50 split.
-- Left panel has its own internal layout: 2 rows.
--   Row 1: 60% + 40%
--   Row 2: full width
-- runChildren hides the inner budget from the parent.
nested :: DomBuilder t m => m ()
nested = ibRow $ G.do
  ib @(F 50 100) leftPanel .>> ib @(F 50 100) rightPanel
  where
    leftPanel = runChildren $ G.do
      ib @(F 60 100) topLeft .>> ib @(F 40 100) topRight
      ib @(F 100 100) bottomFull

    rightPanel = elClass "div" $(classh' [bgColor .~~ solidColor (Purple C50), p .~~ TWSize 6]) $
                   text "Right Panel (50%)"

    topLeft    = elClass "div" $(classh' [bgColor .~~ solidColor (Blue C50), p .~~ TWSize 3]) $
                   text "Top-Left (60% of left)"
    topRight   = elClass "div" $(classh' [bgColor .~~ solidColor (Cyan C50), p .~~ TWSize 3]) $
                   text "Top-Right (40% of left)"
    bottomFull = elClass "div" $(classh' [bgColor .~~ solidColor (Teal C50), p .~~ TWSize 3]) $
                   text "Bottom (100% of left)"


--------------------------------------------------------------------------------
-- Example 6: Compile error demo (uncomment to see the type error)
--------------------------------------------------------------------------------

-- This FAILS at compile time: 70 + 40 = 110 > 100
--
-- overflow :: DomBuilder t m => m ()
-- overflow = ibRow $
--   ib @(F 70 100) (text "70%") .>> ib @(F 40 100) (text "40%")
--
-- GHC error: Couldn't solve: (40 * 1) <= ((100 * 1 - 1 * 70) * 1)
--            i.e. 40 <= 30
