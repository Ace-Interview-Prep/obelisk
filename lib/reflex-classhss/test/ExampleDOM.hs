{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Main where

import qualified Data.ByteString.Char8 as BS8

import Reflex.Dom.Core
import Classh
import Classh.Reflex.InlineBlock
  ( block, blockS, blockRow
  , vblock, vblockCol
  , viewportW, viewportH, viewport, ViewCtx, Dim(..)
  )
import Control.Monad.Tree.Grid.Indexed (N, F, (.>>))
import qualified Control.Monad.Tree.Grid.Indexed as G


main :: IO ()
main = do
  putStrLn "=== Example DOM Output ===\n"

  putStrLn "--- 1) Landing page: header + sidebar/main + footer ---"
  dump landingPage

  putStrLn "\n--- 2) Three-card row ---"
  dump threeCards

  putStrLn "\n--- 3) Asymmetric dashboard: 22% nav + 5% gutter + 73% content ---"
  dump dashboard

  putStrLn "\n--- 4) Nested: 50/50 split, left has two sub-rows ---"
  dump nestedLayout

  putStrLn "\n--- 5) Viewport height: 10% header + 80% main + 10% footer ---"
  dump viewportLayout

  putStrLn "\n--- 6) Combined: width + height viewport ---"
  dump combinedLayout


dump :: (forall t m. DomBuilder t m => m ()) -> IO ()
dump w = do
  (_, bs) <- renderStatic w
  BS8.putStrLn bs
  putStrLn ""


--------------------------------------------------------------------------------
-- 1) Landing page layout
--    Row 1: full-width header
--    Row 2: 25% sidebar + 75% main
--    Row 3: full-width footer
--------------------------------------------------------------------------------

landingPage :: DomBuilder t m => m ()
landingPage = viewportW $ \ctx -> blockRow @(N 100) ctx $ G.do
  block @(F 100 100) header
  block @(F 25 100) sidebar .>> block @(F 75 100) mainContent
  block @(F 100 100) footer
  where
    header = elClass "header" $(classh' [bgColor .~~ solidColor (Indigo C600), p .~~ TWSize 4]) $
               el "h1" $ text "My App"
    sidebar = elClass "nav" $(classh' [bgColor .~~ solidColor (Gray C100), p .~~ TWSize 3]) $
                el "ul" $ do
                  el "li" $ text "Home"
                  el "li" $ text "About"
                  el "li" $ text "Contact"
    mainContent = elClass "main" $(classh' [p .~~ TWSize 6]) $
                    el "p" $ text "Welcome to the main content area."
    footer = elClass "footer" $(classh' [bgColor .~~ solidColor (Gray C800), p .~~ TWSize 3]) $
               text "Footer"


--------------------------------------------------------------------------------
-- 2) Three equal cards using Tailwind-native fractions
--    1/3 + 1/3 + 1/3 = 1
--    Renders as w-1/3 (not w-[33%])
--------------------------------------------------------------------------------

threeCards :: DomBuilder t m => m ()
threeCards = viewportW $ \ctx -> blockRow @(N 1) ctx $
  block @(F 1 3) (card "Plan A" "$9/mo") .>> block @(F 1 3) (card "Plan B" "$19/mo") .>> block @(F 1 3) (card "Plan C" "$49/mo")
  where
    card title price =
      elClass "div" $(classh' [p .~~ TWSize 4, br .~~ R_Lg, shadow .~~ Shadow_Md, bgColor .~~ solidColor White]) $ do
        el "h3" $ text title
        el "p" $ text price


--------------------------------------------------------------------------------
-- 3) Asymmetric dashboard
--    22% + 5% + 73% = 100
--    22 and 73 have no Tailwind fraction — renders as w-[22%], w-[73%]
--------------------------------------------------------------------------------

dashboard :: DomBuilder t m => m ()
dashboard = viewportW $ \ctx -> blockRow @(N 100) ctx $
  block @(F 22 100) navPanel
    .>> block @(F 5 100) gutter
    .>> block @(F 73 100) contentPanel
  where
    navPanel = elClass "aside" $(classh' [bgColor .~~ solidColor (Slate C800), p .~~ TWSize 3]) $
                 el "ul" $ do
                   el "li" $ text "Dashboard"
                   el "li" $ text "Analytics"
                   el "li" $ text "Settings"
    gutter = el "div" blank
    contentPanel = elClass "section" $(classh' [p .~~ TWSize 6]) $ do
                     el "h2" $ text "Analytics Overview"
                     el "p" $ text "Charts and data go here."


--------------------------------------------------------------------------------
-- 4) Nested layout
--    Outer: 50/50 split
--    Left half has its own two rows:
--      Row 1: 60% + 40%
--      Row 2: full width
--------------------------------------------------------------------------------

nestedLayout :: DomBuilder t m => m ()
nestedLayout = viewportW $ \ctx -> blockRow @(N 100) ctx $
  block @(F 50 100) (leftHalf ctx) .>> block @(F 50 100) rightHalf
  where
    leftHalf ctx = do
      blockRow @(N 100) ctx $
        block @(F 60 100) (elClass "div" $(classh' [bgColor .~~ solidColor (Blue C50), p .~~ TWSize 3]) $ text "Top-Left (60%)")
          .>> block @(F 40 100) (elClass "div" $(classh' [bgColor .~~ solidColor (Cyan C50), p .~~ TWSize 3]) $ text "Top-Right (40%)")
      blockRow @(N 100) ctx $
        block @(F 100 100) (elClass "div" $(classh' [bgColor .~~ solidColor (Teal C50), p .~~ TWSize 3]) $ text "Bottom (100%)")

    rightHalf =
      elClass "div" $(classh' [bgColor .~~ solidColor (Purple C50), p .~~ TWSize 6]) $
        text "Right Panel (50%)"


--------------------------------------------------------------------------------
-- 5) Viewport height layout
--    10% header + 80% main + 10% footer = 100% height
--    Uses vblock for compile-time height budget tracking
--------------------------------------------------------------------------------

viewportLayout :: DomBuilder t m => m ()
viewportLayout = viewportH $ \ctx -> vblockCol @(N 100) ctx $ G.do
  vblock @(F 10 100) $
    elClass "header" $(classh' [bgColor .~~ solidColor (Indigo C600), p .~~ TWSize 3]) $
      el "h1" $ text "Header"
  vblock @(F 80 100) $
    elClass "main" $(classh' [p .~~ TWSize 6]) $
      el "p" $ text "Main content area (80% height)"
  vblock @(F 10 100) $
    elClass "footer" $(classh' [bgColor .~~ solidColor (Gray C800), p .~~ TWSize 3]) $
      text "Footer"


--------------------------------------------------------------------------------
-- 6) Combined: width + height viewport
--    Both dimensions are viewport-enforced
--------------------------------------------------------------------------------

combinedLayout :: DomBuilder t m => m ()
combinedLayout = viewport $ \wCtx hCtx ->
  vblockCol @(N 100) hCtx $ G.do
    vblock @(F 10 100) $
      blockRow @(N 100) wCtx $
        block @(F 100 100) $
          elClass "header" $(classh' [bgColor .~~ solidColor (Indigo C600), p .~~ TWSize 3]) $
            el "h1" $ text "My App"
    vblock @(F 80 100) $
      blockRow @(N 100) wCtx $
        block @(F 25 100) (elClass "nav" $(classh' [bgColor .~~ solidColor (Gray C100), p .~~ TWSize 3]) $ text "Sidebar")
          .>> block @(F 75 100) (elClass "main" $(classh' [p .~~ TWSize 6]) $ text "Main content")
    vblock @(F 10 100) $
      blockRow @(N 100) wCtx $
        block @(F 100 100) $
          elClass "footer" $(classh' [bgColor .~~ solidColor (Gray C800), p .~~ TWSize 3]) $
            text "Footer"
