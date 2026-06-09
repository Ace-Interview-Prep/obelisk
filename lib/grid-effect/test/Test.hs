{-# LANGUAGE QualifiedDo #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Main where

import Prelude hiding ((>>=), (>>), return)
import qualified Grid.Effect.Eff as G
import Grid.Effect.Rational

import Effectful (Eff, runPureEff, IOE, runEff)
import Effectful.Dispatch.Static ()

widget :: String -> Eff es String
widget = pure

-- Same row: 4 + 8 = 12 exactly (uses .>> for horizontal)
layout_12col :: G.GridEff (N 12) (N 12) (N 0) es String
layout_12col =
  G.consume @(N 4) (G.liftEff $ widget "sidebar")
    G..>> G.consume @(N 8) (G.liftEff $ widget "main")

-- Fractional: 1/3 + 2/3 = 1 (same row)
layout_thirds :: G.GridEff (N 1) (N 1) '(0, 9) es String
layout_thirds =
  G.consume @(F 1 3) (G.liftEff $ widget "left")
    G..>> G.consume @(F 2 3) (G.liftEff $ widget "right")

-- Multiple rows via G.do (each row gets full budget reset)
layout_rows :: G.GridEff (N 12) (N 12) (N 0) es String
layout_rows = G.do
  -- row 1: 6 + 6 = 12
  _ <- G.consume @(N 6) (G.liftEff $ widget "a") G..>> G.consume @(N 6) (G.liftEff $ widget "b")
  -- row 2: 4 + 8 = 12
  _ <- G.consume @(N 4) (G.liftEff $ widget "c") G..>> G.consume @(N 8) (G.liftEff $ widget "d")
  -- row 3: full width
  G.consume @(N 12) (G.liftEff $ widget "full-width")

-- Nested: 6 cols split into 3+3, then 6 cols flat
layout_nested :: G.GridEff (N 12) (N 12) (N 0) es String
layout_nested =
  G.consume @(N 6) (
    G.consume @(N 3) (G.liftEff $ widget "nested-left")
      G..>> G.consume @(N 3) (G.liftEff $ widget "nested-right")
  ) G..>> G.consume @(N 6) (G.liftEff $ widget "right-half")

-- UNCOMMENT TO SEE COMPILE ERROR (7 + 6 = 13 > 12):
-- layout_overflow :: G.GridEff (N 12) (N 12) (N 0) es ()
-- layout_overflow =
--   G.consume @(N 7) (G.liftEff $ widget "too-wide")
--     G..>> G.consume @(N 6) (G.liftEff $ widget "overflow!")
--   -- ERROR: Couldn't solve: (6 * 1) <= (1 * 5)

main :: IO ()
main = do
  putStrLn $ "12-col: " ++ runPureEff (G.runGridEff_exact layout_12col)
  putStrLn $ "Thirds: " ++ runPureEff (G.runGridEff layout_thirds)
  putStrLn $ "Rows:   " ++ runPureEff (G.runGridEff layout_rows)
  putStrLn $ "Nested: " ++ runPureEff (G.runGridEff_exact layout_nested)
  putStrLn "All type-level budget checks passed at compile time!"
