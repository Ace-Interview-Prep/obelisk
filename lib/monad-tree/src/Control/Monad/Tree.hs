{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Tree
  ( MonadTree(..)
  ) where

import Data.Functor.Identity
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.State.Strict (StateT(..))
import Control.Monad.Trans.Writer.Strict (WriterT(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)


-- | A class for monads that support structural decomposition of product values.
--
-- @split@ decomposes @m (a,b)@ into @(m a, m b)@ with semantics determined
-- by the specific monad:
--
-- * 'Identity' / 'IO': project via @fst@ and @snd@
-- * 'ReaderT': copy the environment to both sides
-- * 'StateT': sequential state consumption
-- * 'WriterT': each side carries the full log
--
-- The returned @(m a, m b)@ are suspended actions. How they get run
-- (sequentially, in parallel, etc.) is handled by separate runner functions.
--
-- No default implementations are provided — each method must be defined
-- per instance, since the semantics of discarding one side of a split
-- are monad-specific.
class Monad m => MonadTree m where

  -- TODO: getBranchView — view the current branch's allocation.
  -- Needs a type family or associated type to know what "the allocation" is.
  -- getBranchView :: m (BranchView m)

  -- | Core primitive: decompose @m (a,b)@ into @(m a, m b)@
  -- preserving effect semantics.
  split    :: m (a,b) -> (m a, m b)

  -- | Split a unit action into two unit actions.
  split_   :: m () -> (m (), m ())

  -- | Split keeping only the first component; discard the second.
  splitFst :: m (a,b) -> (m a, m ())

  -- | Split keeping only the second component; discard the first.
  splitSnd :: m (a,b) -> (m (), m b)


-- | Identity: pure projection, no effects to worry about.
instance MonadTree Identity where
  split m    = (fst <$> m, snd <$> m)
  split_ m   = (m, m)
  splitFst m = (fst <$> m, Identity ())
  splitSnd m = (Identity (), snd <$> m)

-- | IO: same as Identity — project via fst/snd.
instance MonadTree IO where
  split m    = (fst <$> m, snd <$> m)
  split_ m   = (m, m)
  splitFst m = (fst <$> m, pure ())
  splitSnd m = (pure (), snd <$> m)

-- | ReaderT: copy the environment to both sides.
-- The read-only context is not consumed — both children see the same environment.
instance MonadTree m => MonadTree (ReaderT r m) where
  split m = ( ReaderT $ \r -> fst <$> runReaderT m r
            , ReaderT $ \r -> snd <$> runReaderT m r
            )
  split_ m = ( ReaderT $ \r -> runReaderT m r
             , ReaderT $ \r -> runReaderT m r
             )
  splitFst m = ( ReaderT $ \r -> fst <$> runReaderT m r
               , ReaderT $ \_ -> pure ()
               )
  splitSnd m = ( ReaderT $ \_ -> pure ()
               , ReaderT $ \r -> snd <$> runReaderT m r
               )

-- | StateT: sequential consumption.
-- Both sides run the original action to obtain state threading.
-- The first component yields @a@ with the post-action state.
-- The second component yields @b@ with the post-action state.
instance Monad m => MonadTree (StateT s m) where
  split m = ( StateT $ \s -> do { ((a,_), s') <- runStateT m s; pure (a, s') }
            , StateT $ \s -> do { ((_,b), s') <- runStateT m s; pure (b, s') }
            )
  split_ m = ( StateT $ \s -> do { ((), s') <- runStateT m s; pure ((), s') }
             , StateT $ \s -> do { ((), s') <- runStateT m s; pure ((), s') }
             )
  splitFst m = ( StateT $ \s -> do { ((a,_), s') <- runStateT m s; pure (a, s') }
               , StateT $ \s -> do { (_, s') <- runStateT m s; pure ((), s') }
               )
  splitSnd m = ( StateT $ \s -> do { (_, s') <- runStateT m s; pure ((), s') }
               , StateT $ \s -> do { ((_,b), s') <- runStateT m s; pure (b, s') }
               )

-- | WriterT: each side carries the full log from the original action.
-- When the pieces are recombined, logs can be @<>@'d.
instance (Monoid w, Monad m) => MonadTree (WriterT w m) where
  split m = ( WriterT $ do { ((a,_), w) <- runWriterT m; pure (a, w) }
            , WriterT $ do { ((_,b), w) <- runWriterT m; pure (b, w) }
            )
  split_ m = ( WriterT $ do { ((), w) <- runWriterT m; pure ((), w) }
             , WriterT $ do { ((), w) <- runWriterT m; pure ((), w) }
             )
  splitFst m = ( WriterT $ do { ((a,_), w) <- runWriterT m; pure (a, w) }
               , WriterT $ pure ((), mempty)
               )
  splitSnd m = ( WriterT $ pure ((), mempty)
               , WriterT $ do { ((_,b), w) <- runWriterT m; pure (b, w) }
               )

-- | MaybeT: explicitly case on Nothing/Just.
-- Nothing propagates to both sides.
instance Monad m => MonadTree (MaybeT m) where
  split m =
    ( MaybeT $ do
        result <- runMaybeT m
        case result of
          Nothing    -> pure Nothing
          Just (a,_) -> pure (Just a)
    , MaybeT $ do
        result <- runMaybeT m
        case result of
          Nothing    -> pure Nothing
          Just (_,b) -> pure (Just b)
    )
  split_ m =
    ( MaybeT $ do
        result <- runMaybeT m
        case result of
          Nothing -> pure Nothing
          Just () -> pure (Just ())
    , MaybeT $ do
        result <- runMaybeT m
        case result of
          Nothing -> pure Nothing
          Just () -> pure (Just ())
    )
  splitFst m =
    ( MaybeT $ do
        result <- runMaybeT m
        case result of
          Nothing    -> pure Nothing
          Just (a,_) -> pure (Just a)
    , MaybeT $ do
        result <- runMaybeT m
        case result of
          Nothing -> pure Nothing
          Just _  -> pure (Just ())
    )
  splitSnd m =
    ( MaybeT $ do
        result <- runMaybeT m
        case result of
          Nothing -> pure Nothing
          Just _  -> pure (Just ())
    , MaybeT $ do
        result <- runMaybeT m
        case result of
          Nothing    -> pure Nothing
          Just (_,b) -> pure (Just b)
    )

-- | ExceptT: explicitly case on Left/Right.
-- Left propagates to both sides.
instance Monad m => MonadTree (ExceptT e m) where
  split m =
    ( ExceptT $ do
        result <- runExceptT m
        case result of
          Left e      -> pure (Left e)
          Right (a,_) -> pure (Right a)
    , ExceptT $ do
        result <- runExceptT m
        case result of
          Left e      -> pure (Left e)
          Right (_,b) -> pure (Right b)
    )
  split_ m =
    ( ExceptT $ do
        result <- runExceptT m
        case result of
          Left e  -> pure (Left e)
          Right () -> pure (Right ())
    , ExceptT $ do
        result <- runExceptT m
        case result of
          Left e  -> pure (Left e)
          Right () -> pure (Right ())
    )
  splitFst m =
    ( ExceptT $ do
        result <- runExceptT m
        case result of
          Left e      -> pure (Left e)
          Right (a,_) -> pure (Right a)
    , ExceptT $ do
        result <- runExceptT m
        case result of
          Left e -> pure (Left e)
          Right _ -> pure (Right ())
    )
  splitSnd m =
    ( ExceptT $ do
        result <- runExceptT m
        case result of
          Left e -> pure (Left e)
          Right _ -> pure (Right ())
    , ExceptT $ do
        result <- runExceptT m
        case result of
          Left e      -> pure (Left e)
          Right (_,b) -> pure (Right b)
    )
