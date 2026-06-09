{-# LANGUAGE CPP #-}
#if !defined(javascript_HOST_ARCH) && !defined(wasm32_HOST_ARCH)
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
#endif

-- | Bridge from type-level budgets to runtime values.
--
-- The 'ReifiableBudget' class converts a type-level rational into
-- a concrete value type (for CSS class generation, layout rendering, etc.)
module Grid.Effect.Budget
  ( ReifiableBudget(..)
  ) where

import Data.Proxy (Proxy)
import Grid.Effect.Rational (KnownRat)

-- | Types that can be materialized from a type-level rational.
--
-- Implement this for your CSS/layout value type to bridge
-- compile-time budgets to runtime rendering.
--
-- @
-- instance ReifiableBudget TWSizeOrFraction where
--   fromRat p
--     | ratDen p == 1 = TWSize (fromIntegral $ ratNum p)
--     | ratDen p == 2 = TWFraction (fromIntegral $ ratNum p) D2
--     | ...
-- @
class ReifiableBudget b where
  fromRat :: KnownRat r => Proxy r -> b
