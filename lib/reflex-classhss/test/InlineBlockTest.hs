{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QualifiedDo #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Main where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)

import Reflex.Dom.Core

import Classh.Reflex.InlineBlock
  ( block, blockRow, blockRowExact
  , vblock, vblockCol
  , viewportW, viewportH, viewport
  , Inf
  )
import Control.Monad.Tree.Grid.Indexed (N, F, (.>>))
import qualified Control.Monad.Tree.Grid.Indexed as G


main :: IO ()
main = do
  putStrLn "=== InlineBlock Render Tests ==="
  testSingleIb
  testFractionClass
  testPercentFallback
  testIntegerWidth
  testTwoColumnSplit
  testThreeColumns
  testMultiRow
  testNestedLayout
  testZeroWidth
  testInfBudget
  testVblock
  testVblockMultiRow
  testViewportWrapper
  putStrLn "\nAll inline-block render tests passed!"


assert :: String -> Bool -> IO ()
assert label True  = putStrLn $ "  PASS: " ++ label
assert label False = error   $ "  FAIL: " ++ label

-- | Check that a ByteString contains a given substring.
contains :: ByteString -> ByteString -> Bool
contains haystack needle = BS.isInfixOf needle haystack

renderWidget :: (forall t m. (DomBuilder t m) => m ()) -> IO ByteString
renderWidget widget = do
  (_, bs) <- renderStatic widget
  pure bs


--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

-- | block @(F 1 3) renders inline-block w-1/3
testSingleIb :: IO ()
testSingleIb = do
  putStrLn "-- Single block (Tailwind fraction)"
  html <- renderWidget $ viewportW $ \ctx -> blockRow @(N 1) ctx $ block @(F 1 3) (text "hello")
  assert "contains inline-block" (html `contains` "inline-block")
  assert "contains w-1/3"        (html `contains` "w-1/3")
  assert "contains hello"        (html `contains` "hello")

-- | Various Tailwind-native fractions render correctly
testFractionClass :: IO ()
testFractionClass = do
  putStrLn "-- Fraction classes"
  h1 <- renderWidget $ viewportW $ \ctx -> blockRow @(N 1) ctx $ block @(F 1 2) (text "half")
  assert "F 1 2 => w-1/2" (h1 `contains` "w-1/2")

  h2 <- renderWidget $ viewportW $ \ctx -> blockRow @(N 1) ctx $ block @(F 2 3) (text "two-thirds")
  assert "F 2 3 => w-2/3" (h2 `contains` "w-2/3")

  h3 <- renderWidget $ viewportW $ \ctx -> blockRow @(N 1) ctx $ block @(F 3 4) (text "three-quarters")
  assert "F 3 4 => w-3/4" (h3 `contains` "w-3/4")

  h4 <- renderWidget $ viewportW $ \ctx -> blockRow @(N 12) ctx $ block @(F 11 12) (text "eleven-twelfths")
  assert "F 11 12 => w-11/12" (h4 `contains` "w-11/12")

-- | Non-DivInt denominators fall back to percent
testPercentFallback :: IO ()
testPercentFallback = do
  putStrLn "-- Percent fallback"
  h1 <- renderWidget $ viewportW $ \ctx -> blockRow @(N 100) ctx $ block @(F 30 100) (text "thirty")
  assert "F 30 100 => w-[30%]" (h1 `contains` "w-[30%]")

  h2 <- renderWidget $ viewportW $ \ctx -> blockRow @(N 100) ctx $ block @(F 70 100) (text "seventy")
  assert "F 70 100 => w-[70%]" (h2 `contains` "w-[70%]")

  -- 3/7 = 43% (rounded)
  h3 <- renderWidget $ viewportW $ \ctx -> blockRow @(N 1) ctx $ block @(F 3 7) (text "three-sevenths")
  assert "F 3 7 => w-[43%]" (h3 `contains` "w-[43%]")

-- | Integer widths (N n) render as w-n
testIntegerWidth :: IO ()
testIntegerWidth = do
  putStrLn "-- Integer widths"
  h1 <- renderWidget $ viewportW $ \ctx -> blockRow @(N 12) ctx $ block @(N 6) (text "six")
  assert "N 6 => w-6" (h1 `contains` "w-6")

  h2 <- renderWidget $ viewportW $ \ctx -> blockRow @(N 12) ctx $ block @(N 12) (text "twelve")
  assert "N 12 => w-12" (h2 `contains` "w-12")

-- | Two-column split: both divs rendered with correct classes
testTwoColumnSplit :: IO ()
testTwoColumnSplit = do
  putStrLn "-- Two-column split"
  html <- renderWidget $ viewportW $ \ctx -> blockRow @(N 100) ctx $
    block @(F 30 100) (text "left") .>> block @(F 70 100) (text "right")
  assert "left has w-[30%]"  (html `contains` "w-[30%]")
  assert "right has w-[70%]" (html `contains` "w-[70%]")
  assert "both inline-block" (html `contains` "inline-block")
  assert "contains left"     (html `contains` "left")
  assert "contains right"    (html `contains` "right")

-- | Three equal columns
testThreeColumns :: IO ()
testThreeColumns = do
  putStrLn "-- Three equal columns"
  html <- renderWidget $ viewportW $ \ctx -> blockRow @(N 1) ctx $
    block @(F 1 3) (text "A") .>> block @(F 1 3) (text "B") .>> block @(F 1 3) (text "C")
  assert "three w-1/3 divs" (html `contains` "w-1/3")
  assert "contains A"       (html `contains` "A")
  assert "contains B"       (html `contains` "B")
  assert "contains C"       (html `contains` "C")

-- | Multi-row with QualifiedDo: row break resets budget
testMultiRow :: IO ()
testMultiRow = do
  putStrLn "-- Multi-row (QualifiedDo)"
  html <- renderWidget $ viewportW $ \ctx -> blockRow @(N 100) ctx $ G.do
    block @(F 100 100) (text "header")
    block @(F 25 100) (text "sidebar") .>> block @(F 75 100) (text "main")
  assert "header w-[100%]"  (html `contains` "w-[100%]")
  assert "sidebar w-[25%]"  (html `contains` "w-[25%]")
  assert "main w-[75%]"     (html `contains` "w-[75%]")
  assert "contains header"  (html `contains` "header")
  assert "contains sidebar" (html `contains` "sidebar")
  assert "contains main"    (html `contains` "main")

-- | Nested layout: inner blockRow produces plain m (), parent sees it as content
testNestedLayout :: IO ()
testNestedLayout = do
  putStrLn "-- Nested layout"
  html <- renderWidget $ viewportW $ \ctx -> blockRow @(N 100) ctx $
    block @(F 50 100) (innerWidget ctx) .>> block @(F 50 100) (text "right-half")
  assert "outer w-[50%]"       (html `contains` "w-[50%]")
  assert "inner w-[60%]"       (html `contains` "w-[60%]")
  assert "inner w-[40%]"       (html `contains` "w-[40%]")
  assert "inner full w-[100%]" (html `contains` "w-[100%]")
  assert "contains right-half" (html `contains` "right-half")
  where
    -- Each inner blockRow runs its own budget, producing plain m ()
    innerWidget ctx = do
      blockRow @(N 100) ctx $ block @(F 60 100) (text "top-left") .>> block @(F 40 100) (text "top-right")
      blockRow @(N 100) ctx $ block @(F 100 100) (text "bottom-full")

-- | Zero numerator renders w-0
testZeroWidth :: IO ()
testZeroWidth = do
  putStrLn "-- Zero width"
  html <- renderWidget $ viewportW $ \ctx -> blockRow @(N 12) ctx $
    block @(N 0) (text "hidden") .>> block @(N 6) (text "visible")
  assert "N 0 => w-0" (html `contains` "w-0")

-- | Inf budget: any allocation compiles
testInfBudget :: IO ()
testInfBudget = do
  putStrLn "-- Inf budget"
  html <- renderWidget $ viewportW $ \ctx -> blockRow @Inf ctx $
    block @(F 30 100) (text "a") .>> block @(F 70 100) (text "b")
  assert "Inf: contains w-[30%]" (html `contains` "w-[30%]")
  assert "Inf: contains w-[70%]" (html `contains` "w-[70%]")
  assert "Inf: contains a"       (html `contains` "a")
  assert "Inf: contains b"       (html `contains` "b")

-- | vblock renders h- classes, no inline-block
testVblock :: IO ()
testVblock = do
  putStrLn "-- Vertical layout (vblock)"
  html <- renderWidget $ viewportH $ \ctx -> vblockCol @(N 100) ctx $
    vblock @(F 20 100) (text "top") .>> vblock @(F 80 100) (text "bottom")
  assert "vblock h-[20%]"    (html `contains` "h-[20%]")
  assert "vblock h-[80%]"    (html `contains` "h-[80%]")
  assert "contains top"      (html `contains` "top")
  assert "contains bottom"   (html `contains` "bottom")

-- | vblock multi-row with QualifiedDo
testVblockMultiRow :: IO ()
testVblockMultiRow = do
  putStrLn "-- Vertical multi-row (QualifiedDo)"
  html <- renderWidget $ viewportH $ \ctx -> vblockCol @(N 100) ctx $ G.do
    vblock @(F 10 100) (text "header")
    vblock @(F 80 100) (text "main")
    vblock @(F 10 100) (text "footer")
  assert "header h-[10%]"    (html `contains` "h-[10%]")
  assert "main h-[80%]"      (html `contains` "h-[80%]")
  assert "footer h-[10%]"    (html `contains` "h-[10%]")
  assert "contains header"   (html `contains` "header")
  assert "contains main"     (html `contains` "main")
  assert "contains footer"   (html `contains` "footer")

-- | viewport wraps in w-screen h-screen
testViewportWrapper :: IO ()
testViewportWrapper = do
  putStrLn "-- Viewport wrapper"
  html <- renderWidget $ viewport $ \wCtx hCtx -> do
    blockRow @(N 100) wCtx $ block @(F 50 100) (text "left") .>> block @(F 50 100) (text "right")
    vblockCol @(N 100) hCtx $ vblock @(F 100 100) (text "full-height")
  assert "viewport w-screen" (html `contains` "w-screen")
  assert "viewport h-screen" (html `contains` "h-screen")
  assert "contains left"     (html `contains` "left")
  assert "contains right"    (html `contains` "right")
  assert "contains full-height" (html `contains` "full-height")
