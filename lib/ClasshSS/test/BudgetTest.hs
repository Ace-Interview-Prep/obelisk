{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Main where

import Data.Proxy (Proxy(..))
import GHC.TypeLits (Nat)
import qualified Data.Text as T

import Control.Monad.Tree.Grid.Budget (ReifiableBudget(..))
import Control.Monad.Tree.Grid.Indexed (N, F, KnownRat)
import Classh.Box.TWSize.Budget ()
import Classh.Box.TWSize (TWSizeOrFraction)
import Classh.Class.ShowTW (showTW)

main :: IO ()
main = do
  putStrLn "=== ReifiableBudget TWSizeOrFraction Tests ==="
  testInteger
  testZero
  testFractionD2
  testFractionD3
  testFractionD4
  testFractionD5
  testFractionD6
  testFractionD12
  testPercentFallback
  testOddFraction
  putStrLn "\nAll budget tests passed!"

assert :: String -> Bool -> IO ()
assert label True  = putStrLn $ "  PASS: " ++ label
assert label False = error   $ "  FAIL: " ++ label

render :: forall (r :: (Nat, Nat)). KnownRat r => Proxy r -> T.Text
render p = showTW (fromRat p :: TWSizeOrFraction)

testInteger :: IO ()
testInteger = do
  putStrLn "-- Integer rationals"
  assert "N 6 => 6" $ render (Proxy @(N 6)) == "6"
  assert "N 0 => 0" $ render (Proxy @(N 0)) == "0"
  assert "N 12 => 12" $ render (Proxy @(N 12)) == "12"

testZero :: IO ()
testZero = do
  putStrLn "-- Zero numerator"
  assert "'(0, 9) => 0" $ render (Proxy @'(0, 9)) == "0"
  assert "'(0, 100) => 0" $ render (Proxy @'(0, 100)) == "0"

testFractionD2 :: IO ()
testFractionD2 = do
  putStrLn "-- Fractions with D2"
  assert "F 1 2 => 1/2" $ render (Proxy @(F 1 2)) == "1/2"

testFractionD3 :: IO ()
testFractionD3 = do
  putStrLn "-- Fractions with D3"
  assert "F 1 3 => 1/3" $ render (Proxy @(F 1 3)) == "1/3"
  assert "F 2 3 => 2/3" $ render (Proxy @(F 2 3)) == "2/3"

testFractionD4 :: IO ()
testFractionD4 = do
  putStrLn "-- Fractions with D4"
  assert "F 1 4 => 1/4" $ render (Proxy @(F 1 4)) == "1/4"
  assert "F 3 4 => 3/4" $ render (Proxy @(F 3 4)) == "3/4"

testFractionD5 :: IO ()
testFractionD5 = do
  putStrLn "-- Fractions with D5"
  assert "F 2 5 => 2/5" $ render (Proxy @(F 2 5)) == "2/5"

testFractionD6 :: IO ()
testFractionD6 = do
  putStrLn "-- Fractions with D6"
  assert "F 5 6 => 5/6" $ render (Proxy @(F 5 6)) == "5/6"

testFractionD12 :: IO ()
testFractionD12 = do
  putStrLn "-- Fractions with D12"
  assert "F 11 12 => 11/12" $ render (Proxy @(F 11 12)) == "11/12"
  assert "F 7 12 => 7/12" $ render (Proxy @(F 7 12)) == "7/12"

testPercentFallback :: IO ()
testPercentFallback = do
  putStrLn "-- Percent fallback (denominator not in DivInt)"
  assert "F 22 100 => [22%]" $ render (Proxy @(F 22 100)) == "[22%]"
  assert "F 50 100 => [50%]" $ render (Proxy @(F 50 100)) == "[50%]"
  assert "F 75 100 => [75%]" $ render (Proxy @(F 75 100)) == "[75%]"

testOddFraction :: IO ()
testOddFraction = do
  putStrLn "-- Odd denominators (not in DivInt, not 100)"
  -- 3/7 = 42.857...% => rounds to 43
  assert "F 3 7 => [43%]" $ render (Proxy @(F 3 7)) == "[43%]"
  -- 1/8 = 12.5% => rounds to 12 (banker's rounding: round half to even)
  assert "F 1 8 => [12%]" $ render (Proxy @(F 1 8)) == "[12%]"
