{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QualifiedDo #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Main where

-- QualifiedDo resolves >>=, >>, return from the qualified module (G.do).
-- We don't need to hide them from Prelude.

import Control.Monad.Tree.Grid.Indexed
import qualified Control.Monad.Tree.Grid.Indexed as G


main :: IO ()
main = do
  putStrLn "=== GridI (Indexed) Tests ==="
  testSingleConsumeInt
  testSingleConsumeFraction
  testHorizontalChainInt
  testHorizontalChainFraction
  testMixedIntFraction
  testRowBreak
  testNestedScope
  testRunGridI
  testRunGridIExact
  testLiftGridI
  testTargetExample
  putStrLn "\nAll indexed tests passed!"


-- Helpers
assert :: String -> Bool -> IO ()
assert label True  = putStrLn $ "  PASS: " ++ label
assert label False = error   $ "  FAIL: " ++ label


-- | Verify that consuming an integer budget compiles and runs.
-- consume @(N 5) in a 12-column grid leaves N 7.
testSingleConsumeInt :: IO ()
testSingleConsumeInt = do
  putStrLn "-- Single consume (integer)"
  let child :: GridI (N 5) (N 5) (N 5) IO ()
      child = liftGridI (pure ())

      prog :: GridI (N 12) (N 12) (N 7) IO ()
      prog = consume @(N 5) child

  result <- runGridI prog
  assert "consume @(N 5) in N 12 grid compiles and runs" (result == ())


-- | Verify that consuming a fractional budget compiles.
-- consume @(F 1 2) in a total=N 1 grid leaves F 1 2.
testSingleConsumeFraction :: IO ()
testSingleConsumeFraction = do
  putStrLn "-- Single consume (fraction)"
  let child :: GridI (F 1 2) (F 1 2) (F 1 2) IO String
      child = liftGridI (pure "half")

      prog :: GridI (N 1) (N 1) (F 1 2) IO String
      prog = consume @(F 1 2) child

  result <- runGridI prog
  assert "consume @(F 1 2) returns child value" (result == "half")


-- | Two horizontal consumes filling a 12-column grid exactly.
testHorizontalChainInt :: IO ()
testHorizontalChainInt = do
  putStrLn "-- Horizontal chain (integer)"
  let left :: GridI (N 4) (N 4) (N 4) IO ()
      left = liftGridI (pure ())

      right :: GridI (N 8) (N 8) (N 8) IO String
      right = liftGridI (pure "right")

      prog :: GridI (N 12) (N 12) (N 0) IO String
      prog = consume @(N 4) left .>> consume @(N 8) right

  result <- runGridI prog
  assert "4 + 8 = 12, returns right value" (result == "right")


-- | Two fractional consumes: 1/3 + 2/3 = 1.
testHorizontalChainFraction :: IO ()
testHorizontalChainFraction = do
  putStrLn "-- Horizontal chain (fraction)"
  let left :: GridI (F 1 3) (F 1 3) (F 1 3) IO ()
      left = liftGridI (pure ())

      right :: GridI (F 2 3) (F 2 3) (F 2 3) IO String
      right = liftGridI (pure "two-thirds")

      -- After consuming F 1 3 from N 1: remaining = (1*3 - 1*1) / (1*3) = (2, 3)
      -- Then consuming F 2 3 from (2,3): remaining = (2*3 - 3*2) / (3*3) = (0, 9)
      prog :: GridI (N 1) (N 1) '(0, 9) IO String
      prog = consume @(F 1 3) left .>> consume @(F 2 3) right

  result <- runGridI_exact prog
  assert "1/3 + 2/3 = 1 (exact)" (result == "two-thirds")


-- | Mix integer and fraction in the same row.
-- In a 12-column grid: consume 6 (= N 6), then consume 1/2 of 12 (= F 6 1... no).
-- Actually: consume @(N 6) leaves N 6, then consume @(N 6) again.
-- Better test: In a total=N 1 grid, consume F 1 4 then F 3 4.
testMixedIntFraction :: IO ()
testMixedIntFraction = do
  putStrLn "-- Mixed integer and fraction"
  let quarter :: GridI (F 1 4) (F 1 4) (F 1 4) IO ()
      quarter = liftGridI (pure ())

      threeQ :: GridI (F 3 4) (F 3 4) (F 3 4) IO String
      threeQ = liftGridI (pure "three-quarters")

      -- F 1 4 from N 1: remaining = (1*4 - 1*1)/(1*4) = (3, 4)
      -- F 3 4 from (3,4): remaining = (3*4 - 4*3)/(4*4) = (0, 16)
      prog :: GridI (N 1) (N 1) '(0, 16) IO String
      prog = consume @(F 1 4) quarter .>> consume @(F 3 4) threeQ

  result <- runGridI_exact prog
  assert "1/4 + 3/4 = 1 (exact)" (result == "three-quarters")


-- | Row break resets remaining to total.
-- Two rows of 10 in a 12-column grid — each row starts fresh.
testRowBreak :: IO ()
testRowBreak = do
  putStrLn "-- Row break (QualifiedDo)"
  let row1Child :: GridI (N 10) (N 10) (N 10) IO String
      row1Child = liftGridI (pure "row1")

      row2Child :: GridI (N 10) (N 10) (N 10) IO String
      row2Child = liftGridI (pure "row2")

      prog :: GridI (N 12) (N 12) (N 2) IO String
      prog = G.do
        _ <- consume @(N 10) row1Child  -- row 1: 10/12, 2 left (discarded by row break)
        consume @(N 10) row2Child       -- row 2: reset to 12, consume 10, 2 left

  result <- runGridI prog
  assert "row break resets budget" (result == "row2")


-- | Nested scope: parent consumes 5, child runs in a 5-column sub-grid.
testNestedScope :: IO ()
testNestedScope = do
  putStrLn "-- Nested scope (runChildren)"
  let innerLeft :: GridI (N 3) (N 3) (N 3) IO ()
      innerLeft = liftGridI (pure ())

      innerRight :: GridI (N 2) (N 2) (N 2) IO String
      innerRight = liftGridI (pure "inner-right")

      -- runChildren: parent's remaining unchanged (rem_in = rem_out)
      -- The child's internal 3+2=5 is invisible to the parent
      child = runChildren $
        consume @(N 3) innerLeft .>> consume @(N 2) innerRight

      prog :: GridI (N 12) (N 12) (N 7) IO String
      prog = consume @(N 5) child

  result <- runGridI prog
  assert "nested scope: 3+2=5 inside consume 5" (result == "inner-right")


-- | runGridI extracts the underlying monadic value.
testRunGridI :: IO ()
testRunGridI = do
  putStrLn "-- runGridI"
  let prog :: GridI (N 12) (N 12) (N 12) IO Int
      prog = liftGridI (pure 42)
  result <- runGridI prog
  assert "runGridI extracts value" (result == 42)


-- | runGridI_exact requires zero remaining (numerator = 0).
testRunGridIExact :: IO ()
testRunGridIExact = do
  putStrLn "-- runGridI_exact"
  let left :: GridI (N 7) (N 7) (N 7) IO ()
      left = liftGridI (pure ())

      right :: GridI (N 5) (N 5) (N 5) IO Int
      right = liftGridI (pure 100)

      prog :: GridI (N 12) (N 12) (N 0) IO Int
      prog = consume @(N 7) left .>> consume @(N 5) right

  result <- runGridI_exact prog
  assert "runGridI_exact: 7+5=12 exact" (result == 100)


-- | liftGridI doesn't change remaining.
testLiftGridI :: IO ()
testLiftGridI = do
  putStrLn "-- liftGridI"
  let prog :: GridI (N 12) (N 12) (N 12) IO String
      prog = liftGridI (pure "lifted")
  result <- runGridI prog
  assert "liftGridI preserves remaining" (result == "lifted")


-- | Full target example matching the runtime GridM test structure.
--
-- @
-- dom = consume \@(N 5) ma .>> consume \@(N 5) mb
--   where
--     ma = runChildren $ G.do
--       consume \@(N 4) mc .>> consume \@(N 1) md   -- row 1: 4+1=5
--       consume \@(N 3) mc .>> consume \@(N 2) md   -- row 2: 3+2=5
--     mb = liftGridI (return "mb-result")
--     mc = liftGridI (return ())
--     md = liftGridI (return "md")
-- @
testTargetExample :: IO ()
testTargetExample = do
  putStrLn "-- Target example (multi-row nested)"

  let mc4 :: GridI (N 4) (N 4) (N 4) IO ()
      mc4 = liftGridI (pure ())

      md1 :: GridI (N 1) (N 1) (N 1) IO String
      md1 = liftGridI (pure "row1-md")

      mc3 :: GridI (N 3) (N 3) (N 3) IO ()
      mc3 = liftGridI (pure ())

      md2 :: GridI (N 2) (N 2) (N 2) IO String
      md2 = liftGridI (pure "row2-md")

      -- ma: nested 5-column sub-grid with 2 rows
      -- runChildren preserves rem_in = rem_out; child's internal budget is hidden
      ma = runChildren $ G.do
        _ <- consume @(N 4) mc4 .>> consume @(N 1) md1   -- row 1: 4+1=5
        consume @(N 3) mc3 .>> consume @(N 2) md2         -- row 2: 3+2=5

      mb :: GridI (N 5) (N 5) (N 5) IO String
      mb = liftGridI (pure "mb-result")

      -- Top level: consume 5 for ma, consume 5 for mb, in 12-col grid
      dom :: GridI (N 12) (N 12) (N 2) IO String
      dom = consume @(N 5) ma .>> consume @(N 5) mb

  result <- runGridI dom
  -- .>> returns the right-hand value, which is mb
  assert "target example: ma(5) .>> mb(5) in 12-col = mb result" (result == "mb-result")
  putStrLn "    (If this compiled, all budgets are statically verified!)"
