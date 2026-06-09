{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wno-orphans #-}

--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Box.TWSize.Budget
--  Copyright   :  (c) 2024, Galen Sprout
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Galen Sprout <galen.sprout@gmail.com>
--  Stability   :  provisional
--  Portability :  portable
--
--  Bridge between monad-tree's type-level budget tracking and
--  ClasshSS's 'TWSizeOrFraction' width type.
--
--  The orphan instance here is justified: 'ReifiableBudget' is defined
--  in monad-tree and 'TWSizeOrFraction' in ClasshSS. ClasshSS depends
--  on monad-tree, so the instance lives here.
--
--  Example use:
--
-- @
--  fromRat \@'(1, 3) Proxy  ==  TWFraction 1 D3     -- renders as \"w-1\/3\"
--  fromRat \@'(22, 100) Proxy  ==  pct 22            -- renders as \"w-[22%]\"
-- @
--------------------------------------------------------------------------------

module Classh.Box.TWSize.Budget () where

import Control.Monad.Tree.Grid.Budget (ReifiableBudget(..))
import Control.Monad.Tree.Grid.Indexed (KnownRat(..))
import Classh.Box.TWSize
  ( TWSizeOrFraction(..)
  , TWSize(..)
  , intToDivInt
  )
import Classh.Class.HasCSSSize (HasCSSSize(..))

-- | Reify a type-level rational to a 'TWSizeOrFraction' for use as a CSS width.
--
-- Strategy:
--
-- * @n == 0@ => @TWSize\' (TWSize 0)@
-- * @d == 1@ => @TWSize\' (TWSize n)@ (integer size)
-- * @d@ matches a DivInt => @TWFraction n d\'@ (Tailwind-native fraction)
-- * otherwise => @pct (round (100 * n / d))@ (arbitrary percentage)
instance ReifiableBudget TWSizeOrFraction where
  fromRat p
    | n == 0    = TWSize' (TWSize 0)
    | d == 1    = TWSize' (TWSize (fromInteger n))
    | Just di <- intToDivInt d = TWFraction (fromIntegral n) di
    | otherwise = pct (round (100 * fromIntegral n / fromIntegral d :: Double))
    where
      n = ratNum p
      d = ratDen p
