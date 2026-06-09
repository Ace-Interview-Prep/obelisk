{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
module Main where
import Prelude hiding ((>>=), (>>), return)
import qualified Grid.Effect.Eff as G
import Grid.Effect.Rational
import Effectful (Eff, runPureEff)

widget :: String -> Eff es String
widget = pure

-- 7 + 6 = 13 > 12. Should NOT compile.
overflow :: G.GridEff (N 12) (N 12) (N 0) es String
overflow =
  G.consume @(N 7) (G.liftEff $ widget "sidebar")
    G..>> G.consume @(N 6) (G.liftEff $ widget "main")

main :: IO ()
main = putStrLn $ runPureEff $ G.runGridEff overflow
