{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- |
-- Module      : Control.Monad.Tree.Grid.Budget
-- Description : Bridge between type-level rational budgets and value-level budget types
--
-- The 'ReifiableBudget' class connects type-level rationals (used by
-- 'Control.Monad.Tree.Grid.Indexed.GridI' for compile-time tracking)
-- to runtime value-level budget types (like 'ColBudget' or @TWSizeOrFraction@).

module Control.Monad.Tree.Grid.Budget
  ( ReifiableBudget(..)
  ) where

import Data.Proxy (Proxy(..))
import Control.Monad.Consumable (Subtractive)
import Control.Monad.Tree.Grid (ColBudget(..))
import Control.Monad.Tree.Grid.Indexed (KnownRat(..))


-- | Types that can be materialized from a type-level rational.
--
-- This connects the compile-time budget tracking (via type-level @'(Nat, Nat)@)
-- to runtime values. The @Subtractive@ constraint ensures the budget type
-- also supports value-level subtraction for contexts that need it.
--
-- @
-- fromRat \@'(6, 1) Proxy :: ColBudget      -- ColBudget 6
-- fromRat \@'(1, 2) Proxy :: TWSizeOrFraction -- TWFraction 1 D2
-- @
class Subtractive b => ReifiableBudget b where
  fromRat :: KnownRat r => Proxy r -> b

-- | Integer budgets use only the numerator.
instance ReifiableBudget ColBudget where
  fromRat p = ColBudget (fromIntegral (ratNum p))
