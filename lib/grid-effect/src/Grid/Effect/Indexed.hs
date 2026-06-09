{-# LANGUAGE CPP #-}
#if !defined(javascript_HOST_ARCH) && !defined(wasm32_HOST_ARCH)
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
#endif

-- | The core indexed grid monad.
--
-- @GridI total rem_in rem_out m a@ tracks a column budget:
--
-- * @total@   — full budget for the current branch (reset on row-break)
-- * @rem_in@  — remaining when this action starts
-- * @rem_out@ — remaining when this action ends
--
-- At runtime it's just @m a@. All tracking is erased.
module Grid.Effect.Indexed
  ( GridI(..)

    -- * Horizontal sequencing (within a row)
  , (.>>)

    -- * Row-break sequencing (QualifiedDo)
  , (>>=), (>>), return

    -- * Named variants
  , gridBind, gridThen, gridReturn

    -- * Operations
  , consume
  , runChildren
  , runGridI
  , runGridI_exact
  , liftGridI
  ) where

import Prelude hiding ((>>=), (>>), return)
import qualified Prelude

import Grid.Effect.Rational

import GHC.TypeLits (Nat)

infixl 1 .>>

-- | Indexed grid monad. Zero-cost newtype at runtime.
newtype GridI (total :: (Nat, Nat)) (rem_in :: (Nat, Nat)) (rem_out :: (Nat, Nat)) m a
  = GridI { unGridI :: m a }

-- | Horizontal sequencing — same row, budget flows left to right.
(.>>) :: Monad m
      => GridI t i mid m a
      -> GridI t mid j m b
      -> GridI t i j m b
GridI ma .>> GridI mb = GridI (ma Prelude.>> mb)

-- | Row-break bind. Resets remaining to total before continuation.
-- The @forall x@ discards leftover from the first action.
gridBind :: Monad m
         => GridI t i x m a
         -> (a -> GridI t t j m b)
         -> GridI t i j m b
gridBind (GridI ma) f = GridI (ma Prelude.>>= (unGridI . f))

gridThen :: Monad m
         => GridI t i x m a
         -> GridI t t j m b
         -> GridI t i j m b
gridThen (GridI ma) (GridI mb) = GridI (ma Prelude.>> mb)

gridReturn :: Applicative m => a -> GridI t i i m a
gridReturn a = GridI (pure a)

-- QualifiedDo hooks
(>>=) :: Monad m => GridI t i x m a -> (a -> GridI t t j m b) -> GridI t i j m b
(>>=) = gridBind

(>>) :: Monad m => GridI t i x m a -> GridI t t j m b -> GridI t i j m b
(>>) = gridThen

return :: Applicative m => a -> GridI t i i m a
return = gridReturn

-- | Consume @n@ columns from the remaining budget.
--
-- * @RatLeq n rem@ — compile-time check that @n <= rem@
-- * Child runs in a scope of exactly @n@
-- * Parent's remaining decreases by @n@
--
-- @
-- consume \@(N 4) sidebar  -- takes 4 from parent, child has 4 to work with
-- @
consume :: (RatLeq n rem, Monad m)
        => GridI n n child_rem m a
        -> GridI t rem (RatSub rem n) m a
consume (GridI child) = GridI child

-- | Enter a nested scope. Child inherits parent's total.
runChildren :: Monad m
            => GridI t t child_rem m a
            -> GridI t rem rem m a
runChildren (GridI child) = GridI child

-- | Run the grid, discarding leftover.
runGridI :: GridI total total rem_out m a -> m a
runGridI (GridI m) = m

-- | Run the grid, requiring exact consumption (numerator = 0).
runGridI_exact :: (RatNum rem_out ~ 0) => GridI total total rem_out m a -> m a
runGridI_exact (GridI m) = m

-- | Lift an action from the base monad. Budget unchanged.
liftGridI :: m a -> GridI t i i m a
liftGridI = GridI
