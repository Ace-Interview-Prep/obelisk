{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Control.Monad.Consumable
  ( -- * Subtractive
    Subtractive(..)
    -- * Consumable
  , Consumable(..)
  ) where

import Numeric.Natural (Natural)
import Control.Monad.State.Strict (MonadState)


-- | Types that support subtraction with a possibly-different result type.
--
-- The associated type 'Difference' determines the semantics of subtraction:
--
-- * @Difference Natural = Maybe Natural@ — subtraction can fail (underflow)
-- * A clamping type might use @Difference Pct = Pct@ — always succeeds, floors at 0
-- * A 2D budget might use @Difference Grid = Maybe Grid@ — fail if either axis overflows
class Subtractive a where
    type Difference a
    (.-) :: a -> a -> Difference a

infixl 6 .-


instance Subtractive Natural where
    type Difference Natural = Maybe Natural
    (.-) a b
        | b > a     = Nothing
        | otherwise = Just (a - b)

instance Subtractive Int where
    type Difference Int = Int
    (.-) a b = a - b


-- | A monad with a finite resource @s@ that can be consumed in steps.
--
-- Each 'consume' call subtracts from the resource budget (via 'Subtractive')
-- and runs a monadic action within that allocation. The remainder is
-- threaded to subsequent calls via 'MonadState'.
--
-- @
-- -- Budget starts at 12 columns
-- consume (takeC 4) ma >> consume (takeC 5) mb
-- -- After: budget is 3
-- @
class (Subtractive s, MonadState s m) => Consumable s m | m -> s where

  -- | Consume a portion of the resource and run an action.
  -- The first argument specifies how much to take.
  -- The resource in state is updated to the remainder.
  consume :: s -> m a -> m a
