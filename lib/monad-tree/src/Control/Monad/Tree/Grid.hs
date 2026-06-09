{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Tree.Grid
  ( -- * Grid monad
    GridM(..)
  , runGridM
    -- * Budget type
  , ColBudget(..)
    -- * Horizontal sequencing (siblings in a row)
  , (.>>)
    -- * Operations
  , consume
  , runChildren
  , newRow
  , remaining
  , total
  ) where

import Control.Monad.Consumable (Subtractive(..))
import Control.Monad.Tree (MonadTree(..))

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT(..), ask)
import Control.Monad.Trans.State.Strict (StateT(..), get, put)

infixl 1 .>>

-- | Column budget — a count of grid columns.
newtype ColBudget = ColBudget { unColBudget :: Int }
  deriving (Show, Eq, Ord, Num)

-- | Subtraction that fails on underflow.
instance Subtractive ColBudget where
  type Difference ColBudget = Maybe ColBudget
  (.-) (ColBudget a) (ColBudget b)
    | b > a     = Nothing
    | otherwise = Just (ColBudget (a - b))


-- | Grid layout monad.
--
-- * 'ReaderT' holds the total columns for this branch — what 'newRow' restores to.
--   When you 'consume' N columns and enter that branch, the branch's Reader becomes N.
--   So a @col [5]@ branch's \"100%\" is 5, and a 'newRow' inside it restores to 5.
--
-- * 'StateT' holds the remaining columns in the current row.
--   Decremented by 'consume', reset by 'newRow'.
--
-- == Sequencing semantics
--
-- * @>>=@ (do-bind) resets remaining to total before running the continuation.
--   Each statement in a @do@ block is a new row.
--
-- * '.>>' sequences within a row — no reset. Use this for horizontal siblings.
--
-- @
-- dom = consume 5 ma .>> consume 5 mb
--   where
--     ma = runChildren $ do
--       consume 4 mc .>> consume 1 md   -- row 1: 4+1=5
--       consume 3 mc .>> consume 2 md   -- row 2: 3+2=5
-- @
newtype GridM m a = GridM { unGridM :: ReaderT ColBudget (StateT ColBudget m) a }
  deriving (Functor)

-- | Hand-written Applicative: @<*>@ resets remaining to total between
-- the function and argument (consistent with row-break Monad).
instance Monad m => Applicative (GridM m) where
  pure a = GridM (pure a)
  GridM mf <*> GridM ma = GridM $ do
    f <- mf
    -- Row break: reset remaining to total
    t <- ask
    lift $ put t
    a <- ma
    pure (f a)

-- | Hand-written Monad: @>>=@ resets remaining to total before
-- running the continuation. Each do-statement is a new row.
instance Monad m => Monad (GridM m) where
  return = pure
  GridM ma >>= f = GridM $ do
    a <- ma
    -- Row break: reset remaining to total
    t <- ask
    lift $ put t
    -- Run continuation in fresh row
    unGridM (f a)


-- | Horizontal sibling sequencing — no row reset.
--
-- Sequences two actions within the same row. The second action sees
-- whatever remaining budget the first left behind.
--
-- @
-- consume 4 mc .>> consume 1 md   -- both in same row, 4+1=5
-- @
(.>>) :: Monad m => GridM m a -> GridM m b -> GridM m b
GridM ma .>> GridM mb = GridM $ ma Prelude.>> mb


-- | Run a grid layout computation with a given total column count.
-- Returns the result and the final remaining budget.
runGridM :: Monad m => ColBudget -> GridM m a -> m (a, ColBudget)
runGridM cols (GridM m) = runStateT (runReaderT m cols) cols


-- | Consume columns from the current row's budget and run an action
-- within that allocation.
--
-- The child action runs in a new scope where:
--
-- * Reader (total) = the amount consumed (this is the child's \"100%\")
-- * State (remaining) = the amount consumed (fresh budget for the child)
--
-- After the child returns, the parent's remaining budget is decremented
-- by the consumed amount.
--
-- Returns 'Nothing' if the budget would underflow.
consume :: Monad m => ColBudget -> GridM m a -> GridM m (Maybe a)
consume cost child = GridM $ do
  rem_ <- lift get
  case rem_ .- cost of
    Nothing -> pure Nothing
    Just rem' -> do
      -- Run child in its own scope: total=cost, remaining=cost
      let childAction = runReaderT (unGridM child) cost
      result <- lift $ StateT $ \_ -> do
        (a, _childFinalRem) <- runStateT childAction cost
        pure (a, rem')
      pure (Just result)

-- | Enter a nested grid scope.
--
-- The child inherits the current branch's total as both its total and
-- remaining. This establishes a fresh inner grid for the child's rows.
--
-- @
-- ma = runChildren $ do
--   consume 4 mc .>> consume 1 md   -- row 1
--   consume 3 mc .>> consume 2 md   -- row 2
-- @
runChildren :: Monad m => GridM m a -> GridM m a
runChildren child = GridM $ do
  t <- ask
  -- Run child with total=t, remaining=t (fresh scope)
  let childAction = runReaderT (unGridM child) t
  lift $ StateT $ \s -> do
    (a, _childFinalRem) <- runStateT childAction t
    pure (a, s)

-- | Start a new row: reset remaining columns to this branch's total.
--
-- Inside a @col [5]@ branch, 'newRow' restores to 5, not to the
-- grandparent's 12.
--
-- Note: With the custom Monad instance, @>>=@ automatically calls
-- 'newRow' between do-statements. This function is available for
-- explicit use when needed.
newRow :: Monad m => GridM m ()
newRow = GridM $ do
  t <- ask
  lift $ put t

-- | Query how many columns remain in the current row.
remaining :: Monad m => GridM m ColBudget
remaining = GridM $ lift get

-- | Query the total columns for this branch (what 'newRow' restores to).
total :: Monad m => GridM m ColBudget
total = GridM ask


-- | MonadTree instance for GridM.
--
-- 'split' decomposes @GridM m (a,b)@ into @(GridM m a, GridM m b)@.
-- Both sides see the same Reader (total) and run the original action
-- to get state threading.
instance Monad m => MonadTree (GridM m) where
  split m =
    ( GridM $ do
        t <- ask
        s <- lift get
        let inner = runReaderT (unGridM m) t
        lift $ StateT $ \_ -> do
          ((a, _), s') <- runStateT inner s
          pure (a, s')
    , GridM $ do
        t <- ask
        s <- lift get
        let inner = runReaderT (unGridM m) t
        lift $ StateT $ \_ -> do
          ((_, b), s') <- runStateT inner s
          pure (b, s')
    )

  split_ m =
    ( GridM $ do
        t <- ask
        s <- lift get
        let inner = runReaderT (unGridM m) t
        lift $ StateT $ \_ -> do
          ((), s') <- runStateT inner s
          pure ((), s')
    , GridM $ do
        t <- ask
        s <- lift get
        let inner = runReaderT (unGridM m) t
        lift $ StateT $ \_ -> do
          ((), s') <- runStateT inner s
          pure ((), s')
    )

  splitFst m =
    ( GridM $ do
        t <- ask
        s <- lift get
        let inner = runReaderT (unGridM m) t
        lift $ StateT $ \_ -> do
          ((a, _), s') <- runStateT inner s
          pure (a, s')
    , GridM $ do
        t <- ask
        s <- lift get
        let inner = runReaderT (unGridM m) t
        lift $ StateT $ \_ -> do
          (_, s') <- runStateT inner s
          pure ((), s')
    )

  splitSnd m =
    ( GridM $ do
        t <- ask
        s <- lift get
        let inner = runReaderT (unGridM m) t
        lift $ StateT $ \_ -> do
          (_, s') <- runStateT inner s
          pure ((), s')
    , GridM $ do
        t <- ask
        s <- lift get
        let inner = runReaderT (unGridM m) t
        lift $ StateT $ \_ -> do
          ((_, b), s') <- runStateT inner s
          pure (b, s')
    )
