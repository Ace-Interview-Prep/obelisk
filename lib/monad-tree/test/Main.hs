module Main where

import Data.Functor.Identity
import Control.Monad.Trans.State.Strict (StateT, runStateT, modify', get)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans.Writer.Strict (WriterT, runWriterT, tell)

import Control.Monad.Tree
import Control.Monad.Tree.Helpers
import Control.Monad.Tree.Grid
import Control.Monad.Consumable hiding (consume)


main :: IO ()
main = do
  putStrLn "=== MonadTree Tests ==="
  testIdentitySplit
  testReaderTSplit
  testStateTSplit
  testWriterTSplit
  testHelpers

  putStrLn "\n=== Subtractive Tests ==="
  testSubtractive

  putStrLn "\n=== GridM Tests ==="
  testGridConsume
  testGridOverflow
  testGridRowReset
  testGridRunChildren
  testGridNestedBudget
  testGridTargetExample

  putStrLn "\nAll tests passed!"


-- Helpers
assert :: String -> Bool -> IO ()
assert label True  = putStrLn $ "  PASS: " ++ label
assert label False = error   $ "  FAIL: " ++ label


-- === MonadTree: Identity ===

testIdentitySplit :: IO ()
testIdentitySplit = do
  putStrLn "-- Identity split"
  let m = Identity (1 :: Int, 2 :: Int)
      (ma, mb) = split m
  assert "fst" (runIdentity ma == 1)
  assert "snd" (runIdentity mb == 2)

  let (ma', _) = splitFst m
  assert "splitFst fst" (runIdentity ma' == 1)

  let (_, mb') = splitSnd m
  assert "splitSnd snd" (runIdentity mb' == 2)


-- === MonadTree: ReaderT ===

testReaderTSplit :: IO ()
testReaderTSplit = do
  putStrLn "-- ReaderT split"
  let m :: ReaderT Int Identity (Int, Int)
      m = do
        r <- ask
        pure (r + 1, r + 2)
      (ma, mb) = split m
  assert "fst sees env" (runIdentity (runReaderT ma 10) == 11)
  assert "snd sees env" (runIdentity (runReaderT mb 10) == 12)


-- === MonadTree: StateT ===

testStateTSplit :: IO ()
testStateTSplit = do
  putStrLn "-- StateT split"
  let m :: StateT Int Identity (Int, Int)
      m = do
        modify' (+1)
        s <- get
        pure (s, s * 2)
      (ma, mb) = split m
  -- Both run the action from initial state 0
  assert "fst value" (runIdentity (runStateT ma 0) == (1, 1))
  assert "snd value" (runIdentity (runStateT mb 0) == (2, 1))
  assert "fst state threaded" (snd (runIdentity (runStateT ma 0)) == 1)
  assert "snd state threaded" (snd (runIdentity (runStateT mb 0)) == 1)


-- === MonadTree: WriterT ===

testWriterTSplit :: IO ()
testWriterTSplit = do
  putStrLn "-- WriterT split"
  let m :: WriterT String Identity (Int, Int)
      m = do
        tell "ab"
        pure (1, 2)
      (ma, mb) = split m
  assert "fst carries log" (runIdentity (runWriterT ma) == (1, "ab"))
  assert "snd carries log" (runIdentity (runWriterT mb) == (2, "ab"))

  let (_, mb') = splitSnd m
  assert "splitSnd discards fst, snd keeps log" (runIdentity (runWriterT mb') == (2, "ab"))

  let (ma', _) = splitFst m
  assert "splitFst discards snd, fst keeps log" (runIdentity (runWriterT ma') == (1, "ab"))


-- === Helpers: f3, f4 ===

testHelpers :: IO ()
testHelpers = do
  putStrLn "-- N-ary helpers"
  let m3 = Identity (1 :: Int, (2 :: Int, 3 :: Int))
      (a, b, c) = f3 m3
  assert "f3 fst" (runIdentity a == 1)
  assert "f3 snd" (runIdentity b == 2)
  assert "f3 trd" (runIdentity c == 3)

  let m4 = Identity (10 :: Int, (20 :: Int, (30 :: Int, 40 :: Int)))
      (w, x, y, z) = f4 m4
  assert "f4 1st" (runIdentity w == 10)
  assert "f4 2nd" (runIdentity x == 20)
  assert "f4 3rd" (runIdentity y == 30)
  assert "f4 4th" (runIdentity z == 40)

  -- Roundtrip: f3 . f3'
  let flat3 = Identity (1 :: Int, 2 :: Int, 3 :: Int)
      (a', b', c') = f3 (f3' flat3)
  assert "f3 . f3' roundtrip" (runIdentity a' == 1 && runIdentity b' == 2 && runIdentity c' == 3)


-- === Subtractive ===

testSubtractive :: IO ()
testSubtractive = do
  putStrLn "-- Subtractive ColBudget"
  assert "10 .- 3 = Just 7" (ColBudget 10 .- ColBudget 3 == Just (ColBudget 7))
  assert "3 .- 10 = Nothing" (ColBudget 3 .- ColBudget 10 == Nothing)
  assert "5 .- 5 = Just 0"  (ColBudget 5 .- ColBudget 5 == Just (ColBudget 0))


-- === GridM: consume ===

testGridConsume :: IO ()
testGridConsume = do
  putStrLn "-- GridM consume"
  -- Test single consume
  (r1, rem1) <- runGridM 12 (consume 5 remaining)
  putStrLn $ "    single consume 5: result=" ++ show r1 ++ " rem=" ++ show rem1
  assert "single consume child sees 5" (r1 == Just (ColBudget 5))
  assert "single consume remaining 7" (rem1 == ColBudget 7)

  -- Test two consumes with .>>
  (r2, rem2) <- runGridM 12 (consume 5 (pure ()) .>> consume 3 remaining)
  putStrLn $ "    consume 5 .>> consume 3: result=" ++ show r2 ++ " rem=" ++ show rem2
  assert "second child sees 3" (r2 == Just (ColBudget 3))
  assert "parent remaining after both" (rem2 == ColBudget 4)


-- === GridM: overflow ===

testGridOverflow :: IO ()
testGridOverflow = do
  putStrLn "-- GridM overflow"
  let prog = consume 10 (pure "ok") .>> consume 5 (pure "bad")
  (result, finalRem) <- runGridM 12 prog
  -- 10 from 12 -> 2 left, then try 5 from 2 -> Nothing
  assert "overflow returns Nothing" (result == Nothing)
  assert "remaining after overflow" (finalRem == ColBudget 2)


-- === GridM: row reset via >>= ===

testGridRowReset :: IO ()
testGridRowReset = do
  putStrLn "-- GridM row reset (do-bind)"
  -- In do-block, >>= resets remaining to total between statements
  let prog = do
        _ <- consume 10 (pure ()) .>> consume 2 (pure ())
        -- New row: remaining resets to 12
        consume 8 remaining
  (result, _) <- runGridM 12 prog
  -- After row reset, child should see 8 (its allocation), not 0
  assert "row reset restores budget" (result == Just (ColBudget 8))


-- === GridM: runChildren ===

testGridRunChildren :: IO ()
testGridRunChildren = do
  putStrLn "-- GridM runChildren"
  let child = runChildren $ do
        consume 3 remaining .>> consume 2 remaining
  -- Parent consumes 5 for the child, child has total=5
  let prog = consume 5 child
  (result, finalRem) <- runGridM 12 prog
  putStrLn $ "    runChildren result=" ++ show result ++ " finalRem=" ++ show finalRem
  -- .>> returns the right-hand result:
  -- consume 3: takes 3 from 5, child sees remaining=3
  -- consume 2: takes 2 from 2, child sees remaining=2 (this is the returned value)
  assert "runChildren last child sees 2" (result == Just (Just (ColBudget 2)))
  assert "parent remaining" (finalRem == ColBudget 7)


-- === GridM: nested budget scoping ===

testGridNestedBudget :: IO ()
testGridNestedBudget = do
  putStrLn "-- GridM nested budget"
  -- Inside a consume 5 branch, newRow should restore to 5, not 12
  let child = runChildren $ do
        _ <- consume 5 remaining  -- row 1: takes all 5
        -- row 2: >>= resets to 5 (branch total)
        consume 3 remaining       -- takes 3 from fresh 5
  let prog = consume 5 child
  (result, finalRem) <- runGridM 12 prog
  putStrLn $ "    nested budget result=" ++ show result ++ " finalRem=" ++ show finalRem
  assert "nested row resets to branch total" (result == Just (Just (ColBudget 3)))


-- === GridM: full target example ===

testGridTargetExample :: IO ()
testGridTargetExample = do
  putStrLn "-- GridM target example"
  -- dom = consume 5 ma .>> consume 5 mb
  --   where ma = runChildren $ do
  --           consume 4 mc .>> consume 1 md
  --           consume 3 mc .>> consume 2 md
  --           consume 2 mc .>> consume 3 md
  --           consume 1 mc .>> consume 4 md
  --         mc = remaining  -- reports what it sees
  --         md = remaining

  let mc = remaining
      md = remaining
      ma = runChildren $ do
        _ <- consume 4 mc .>> consume 1 md
        _ <- consume 3 mc .>> consume 2 md
        _ <- consume 2 mc .>> consume 3 md
        consume 1 mc .>> consume 4 md
      mb = remaining
      dom = consume 5 ma .>> consume 5 mb

  (result, finalRem) <- runGridM 12 dom
  putStrLn $ "    target example result=" ++ show result ++ " finalRem=" ++ show finalRem

  -- ma consumes 5, mb consumes 5, 2 remaining
  assert "final remaining is 2" (finalRem == ColBudget 2)

  -- mb (second sibling) sees its total=5 as remaining
  assert "mb sees 5" (result == Just (ColBudget 5))
