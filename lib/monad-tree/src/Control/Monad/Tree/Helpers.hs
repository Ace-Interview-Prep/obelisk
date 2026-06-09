module Control.Monad.Tree.Helpers
  ( -- * N-ary decomposition via right-nested tuples
    f3
  , f4
  , f5
    -- * Isomorphisms: flat tuple -> right-nested
  , f3'
  , f4'
  , f5'
  ) where

import Control.Monad.Tree (MonadTree(..))


-- | Decompose @m (a,(b,c))@ into @(m a, m b, m c)@ via iterated 'split'.
f3 :: MonadTree m => m (a,(b,c)) -> (m a, m b, m c)
f3 m =
  let (ma, mbc) = split m
      (mb, mc)  = split mbc
  in (ma, mb, mc)

-- | Decompose @m (a,(b,(c,d)))@ into @(m a, m b, m c, m d)@.
f4 :: MonadTree m => m (a,(b,(c,d))) -> (m a, m b, m c, m d)
f4 m =
  let (ma, mbcd)    = split m
      (mb, mc, md)  = f3 mbcd
  in (ma, mb, mc, md)

-- | Decompose @m (a,(b,(c,(d,e))))@ into @(m a, m b, m c, m d, m e)@.
f5 :: MonadTree m => m (a,(b,(c,(d,e)))) -> (m a, m b, m c, m d, m e)
f5 m =
  let (ma, mbcde)       = split m
      (mb, mc, md, me)  = f4 mbcde
  in (ma, mb, mc, md, me)


-- | Right-nest a flat 3-tuple: @m (a, b, c)@ -> @m (a,(b,c))@.
f3' :: Functor m => m (a, b, c) -> m (a,(b,c))
f3' = fmap (\(a,b,c) -> (a,(b,c)))

-- | Right-nest a flat 4-tuple: @m (a, b, c, d)@ -> @m (a,(b,(c,d)))@.
f4' :: Functor m => m (a, b, c, d) -> m (a,(b,(c,d)))
f4' = fmap (\(a,b,c,d) -> (a,(b,(c,d))))

-- | Right-nest a flat 5-tuple: @m (a, b, c, d, e)@ -> @m (a,(b,(c,(d,e))))@.
f5' :: Functor m => m (a, b, c, d, e) -> m (a,(b,(c,(d,e))))
f5' = fmap (\(a,b,c,d,e) -> (a,(b,(c,(d,e)))))
