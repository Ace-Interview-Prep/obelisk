{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NoStarIsType #-}

#if !defined(javascript_HOST_ARCH) && !defined(wasm32_HOST_ARCH)
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
#endif

-- |
-- Module      : Control.Monad.Tree.Grid.Indexed
-- Description : Compile-time grid budget tracking via an indexed monad with type-level rationals
--
-- This module provides 'GridI', an indexed monad that tracks grid column
-- budgets at the type level. Overflow is a compile-time type error.
--
-- Budgets are type-level rationals encoded as @'(Nat, Nat)@ pairs
-- (numerator, denominator), supporting both integer column counts and
-- fractional layouts.
--
-- == Usage with QualifiedDo
--
-- @
-- {-\# LANGUAGE QualifiedDo \#-}
-- import qualified Control.Monad.Tree.Grid.Indexed as G
--
-- dom :: GridI (N 12) (N 12) (N 0) IO ()
-- dom = G.do
--   consume \@(N 4) mc .>> consume \@(N 8) md    -- row 1
--   consume \@(N 6) mc .>> consume \@(N 6) md    -- row 2
-- @

module Control.Monad.Tree.Grid.Indexed
  ( -- * Type-level rationals
    N
  , F
  , Inf
  , RatNum
  , RatDen
  , RatLeq
  , RatSub

    -- * Reification
  , KnownRat(..)

    -- * Indexed grid monad
  , GridI(..)

    -- * Horizontal sequencing (within a row)
  , (.>>)

    -- * Row-break sequencing (for QualifiedDo)
  , (>>=)
  , (>>)
  , return

    -- * Named alternatives
  , gridBind
  , gridThen
  , gridReturn

    -- * Operations
  , consume
  , runChildren
  , runGridI
  , runGridI_exact
  , liftGridI
  ) where

import Prelude hiding ((>>=), (>>), return)
import qualified Prelude

import Data.Proxy (Proxy(..))
import Data.Kind (Constraint)
import GHC.TypeLits (Nat, KnownNat, natVal)
import GHC.TypeNats (type (<=), type (-), type (*))


--------------------------------------------------------------------------------
-- Type-level rationals
--------------------------------------------------------------------------------

-- | Integer as a rational: @N 12 = '(12, 1) = 12/1@
type N (n :: Nat) = '(n, 1)

-- | Fraction: @F 1 2 = '(1, 2) = 1/2@
type F (n :: Nat) (d :: Nat) = '(n, d)

-- | Unbounded budget. Any realistic allocation satisfies @RatLeq n Inf@.
--
-- Use with 'blockRow' or 'vblockCol' when you don't want compile-time
-- width/height tracking:
--
-- @
-- blockRow \@Inf $ block \@(F 30 100) sidebar .>> block_ TWSize_Auto main_
-- @
type Inf = N 1000000

-- | Extract the numerator of a type-level rational.
type family RatNum (r :: (Nat, Nat)) :: Nat where
  RatNum '(n, _) = n

-- | Extract the denominator of a type-level rational.
type family RatDen (r :: (Nat, Nat)) :: Nat where
  RatDen '(_, d) = d

-- | Rational comparison: @a/b <= c/d@ iff @a*d <= b*c@ (cross-multiply).
--
-- This expands to a GHC @(<=)@ constraint on Nats, so overflow
-- produces a clear compile-time error like @Couldn't solve: 6 <= 4@.
type family RatLeq (x :: (Nat, Nat)) (y :: (Nat, Nat)) :: Constraint where
  RatLeq '(a, b) '(c, d) = (a * d) <= (c * b)

-- | Rational subtraction: @a/b - c/d = (a*d - b*c) / (b*d)@.
--
-- Safe when guarded by 'RatLeq' — the @(-)@ on Nats won't underflow.
type family RatSub (x :: (Nat, Nat)) (y :: (Nat, Nat)) :: (Nat, Nat) where
  RatSub '(a, b) '(c, d) = '(a * d - b * c, b * d)


--------------------------------------------------------------------------------
-- KnownRat — reify type-level rationals at runtime
--------------------------------------------------------------------------------

-- | Witness that both components of a type-level rational are known.
class (KnownNat (RatNum r), KnownNat (RatDen r)) => KnownRat (r :: (Nat, Nat)) where
  -- | The numerator as a runtime Integer.
  ratNum :: Proxy r -> Integer
  ratNum _ = natVal (Proxy @(RatNum r))

  -- | The denominator as a runtime Integer.
  ratDen :: Proxy r -> Integer
  ratDen _ = natVal (Proxy @(RatDen r))

  -- | The rational value as a runtime Float.
  ratToFloat :: Proxy r -> Float
  ratToFloat p = fromIntegral (ratNum p) / fromIntegral (ratDen p)

instance (KnownNat n, KnownNat d) => KnownRat '(n, d)


--------------------------------------------------------------------------------
-- GridI — the indexed grid monad
--------------------------------------------------------------------------------

infixl 1 .>>

-- | An indexed monad for compile-time grid budget tracking.
--
-- * @total@   — the branch's column budget (what row-break resets to)
-- * @rem_in@  — columns remaining when this action begins
-- * @rem_out@ — columns remaining when this action ends
--
-- At runtime this is just @m a@ — zero overhead. All budget tracking
-- is erased by GHC.
newtype GridI (total :: (Nat, Nat)) (rem_in :: (Nat, Nat)) (rem_out :: (Nat, Nat)) m a
  = GridI { unGridI :: m a }


-- | Horizontal sequencing — no row reset.
--
-- Threads the remaining budget from the first action to the second,
-- both within the same row.
--
-- @
-- consume \@(N 4) mc .>> consume \@(N 8) md   -- both in same row
-- @
(.>>) :: Monad m
      => GridI t i mid m a
      -> GridI t mid j m b
      -> GridI t i j m b
GridI ma .>> GridI mb = GridI (ma Prelude.>> mb)


--------------------------------------------------------------------------------
-- Row-break sequencing (for QualifiedDo)
--------------------------------------------------------------------------------

-- | Row-break bind: resets remaining to total before the continuation.
--
-- Each statement in a @G.do@ block is a new row.
-- The @forall x@ universally quantifies the leftover from the first
-- action — it's discarded by the row break.
gridBind :: Monad m
         => GridI t i x m a
         -> (a -> GridI t t j m b)
         -> GridI t i j m b
gridBind (GridI ma) f = GridI (ma Prelude.>>= (unGridI . f))

-- | Row-break then: like 'gridBind' but discards the first result.
gridThen :: Monad m
         => GridI t i x m a
         -> GridI t t j m b
         -> GridI t i j m b
gridThen (GridI ma) (GridI mb) = GridI (ma Prelude.>> mb)

-- | Lift a pure value into GridI. Remaining unchanged.
gridReturn :: Applicative m => a -> GridI t i i m a
gridReturn a = GridI (pure a)

-- | QualifiedDo: @G.do { x <- a; b }@ desugars to @(G.>>=) a (\\x -> b)@
(>>=) :: Monad m
      => GridI t i x m a
      -> (a -> GridI t t j m b)
      -> GridI t i j m b
(>>=) = gridBind

-- | QualifiedDo: @G.do { a; b }@ desugars to @(G.>>) a b@
(>>) :: Monad m
     => GridI t i x m a
     -> GridI t t j m b
     -> GridI t i j m b
(>>) = gridThen

-- | QualifiedDo: @G.return x@
return :: Applicative m => a -> GridI t i i m a
return = gridReturn


--------------------------------------------------------------------------------
-- Operations
--------------------------------------------------------------------------------

-- | Consume a portion of the budget and run a child action within that allocation.
--
-- * The child's total and starting remaining are both @n@ (the consumed amount).
-- * The parent's remaining decreases by @n@.
-- * @RatLeq n rem@ ensures @n <= rem@ at compile time — overflow is a type error.
-- * Returns @a@ directly, not @Maybe a@: impossible states are unrepresentable.
--
-- At runtime this is the identity function.
--
-- @
-- consume \@(N 5) child   -- takes 5 from parent, child runs in a 5-column scope
-- consume \@(F 1 2) child -- takes 1/2 from parent
-- @
consume :: (RatLeq n rem, Monad m)
        => GridI n n child_ro m a
        -> GridI t rem (RatSub rem n) m a
consume (GridI child) = GridI child

-- | Enter a nested grid scope.
--
-- The child inherits the current branch's total as both its total and
-- remaining. Parent's remaining is unchanged.
--
-- @
-- ma = runChildren $ G.do
--   consume \@(N 4) mc .>> consume \@(N 1) md   -- row 1
--   consume \@(N 3) mc .>> consume \@(N 2) md   -- row 2
-- @
runChildren :: Monad m
            => GridI t t child_ro m a
            -> GridI t rem rem m a
runChildren (GridI child) = GridI child

-- | Run a grid computation, discarding the type-level budget.
-- Leftover budget is allowed.
runGridI :: GridI total total rem_out m a -> m a
runGridI (GridI m) = m

-- | Run a grid computation, requiring that the entire budget is consumed.
-- @rem_out@ must be @'(0, d)@ for some @d@ — i.e., the numerator is 0.
--
-- For integer grids: @runGridI_exact :: GridI (N 12) (N 12) (N 0) m a -> m a@
runGridI_exact :: (RatNum rem_out ~ 0) => GridI total total rem_out m a -> m a
runGridI_exact (GridI m) = m

-- | Lift an action from the underlying monad into GridI.
-- Remaining budget is unchanged.
liftGridI :: m a -> GridI t i i m a
liftGridI = GridI
