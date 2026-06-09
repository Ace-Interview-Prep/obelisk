{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Reflex.InlineBlock
--  Copyright   :  (c) 2024, Galen Sprout
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Galen Sprout <galen.sprout@gmail.com>
--  Stability   :  provisional
--  Portability :  portable
--
--  Compile-time budget-tracked inline-block layout with viewport enforcement.
--
--  All layouts must begin with a viewport runner ('viewportW', 'viewportH',
--  or 'viewport') which establishes a sized parent and provides an opaque
--  'ViewCtx' token. 'blockRow' and 'vblockCol' require this token — calling
--  them without a viewport runner is a compile error.
--
--  == Example
--
--  @
--  myApp = viewport $ \\w h -> do
--    blockRow \@(N 100) w $
--      block \@(F 30 100) (sidebar w) .>> block \@(F 70 100) (mainContent w h)
--
--  sidebar w = blockRow \@(N 100) w $
--    block \@(F 50 100) (text "nav") .>> block \@(F 50 100) (text "filters")
--
--  mainContent w h = vblockCol \@(N 100) h $ G.do
--    vblock \@(F 10 100) (text "header")
--    vblock \@(F 80 100) $ blockRow \@(N 100) w $
--      block \@(F 60 100) (text "charts") .>> block \@(F 40 100) (text "stats")
--    vblock \@(F 10 100) (text "footer")
--  @
--------------------------------------------------------------------------------

module Classh.Reflex.InlineBlock
  ( -- * Viewport context
    Dim(..)
  , ViewCtx   -- type exported, constructor NOT exported
  , viewportW
  , viewportH
  , viewport

    -- * Horizontal layout (width-tracked)
  , block
  , blockS
  , blockCell
  , blockCellS

    -- * Horizontal row runners
  , blockRow
  , blockRowExact

    -- * Vertical layout (height-tracked)
  , vblock
  , vblockS
  , vblockCell
  , vblockCellS

    -- * Vertical column runners
  , vblockCol
  , vblockColExact

    -- * Re-exports
  , Inf
  ) where

import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import GHC.TypeLits (Nat)

import Reflex.Dom.Core (DomBuilder, elClass)

import Classh
import Classh.Box.TWSize.Budget ()

import Control.Monad.Tree.Grid.Budget (ReifiableBudget(..))
import Control.Monad.Tree.Grid.Indexed
  ( GridI, Inf, KnownRat, RatLeq, RatSub, RatNum
  , consume, liftGridI, runGridI, runGridI_exact
  )


--------------------------------------------------------------------------------
-- Viewport context (opaque capability token)
--------------------------------------------------------------------------------

-- | Dimension tag for 'ViewCtx'.
data Dim = W | H

-- | Opaque proof that a viewport-sized parent exists.
--
-- The constructor is NOT exported — the only way to obtain a 'ViewCtx'
-- is through 'viewportW', 'viewportH', or 'viewport'.
newtype ViewCtx (d :: Dim) = ViewCtx ()

-- | Establish a width viewport context (@w-screen@).
--
-- Wraps content in a @div@ with @width: 100vw@, then passes
-- a 'ViewCtx' \'W token to the callback.
viewportW :: DomBuilder reflex m => (ViewCtx 'W -> m a) -> m a
viewportW f = elClass "div" $(classh' [w .~~ TWSize_Screen]) (f (ViewCtx ()))

-- | Establish a height viewport context (@h-screen@).
--
-- Wraps content in a @div@ with @height: 100vh@, then passes
-- a 'ViewCtx' \'H token to the callback.
viewportH :: DomBuilder reflex m => (ViewCtx 'H -> m a) -> m a
viewportH f = elClass "div" $(classh' [h .~~ TWSize_Screen]) (f (ViewCtx ()))

-- | Establish both width and height viewport context.
--
-- Wraps content in a @div@ with @width: 100vw; height: 100vh@.
viewport :: DomBuilder reflex m => (ViewCtx 'W -> ViewCtx 'H -> m a) -> m a
viewport f = elClass "div" $(classh' [w .~~ TWSize_Screen, h .~~ TWSize_Screen]) (f (ViewCtx ()) (ViewCtx ()))


--------------------------------------------------------------------------------
-- Horizontal layout (width-tracked, inline-block)
--------------------------------------------------------------------------------

-- | Wrap a widget in an @inline-block@ div with width derived from
-- the type-level rational @n@.
--
-- This does NOT consume budget — use 'block' for budget-tracked layout.
-- Use 'blockCell' when you need the cell wrapper without budget accounting
-- (e.g. inside 'runChildren').
blockCell :: forall (n :: (Nat, Nat)) reflex m a.
             (KnownRat n, DomBuilder reflex m)
          => m a -> GridI n n n m a
blockCell child = liftGridI $
  elClass "div" ("inline-block" <> " " <> widthClass @n) child

-- | Like 'blockCell' but also applies ClasshSS styling.
blockCellS :: forall (n :: (Nat, Nat)) reflex m a.
              (KnownRat n, DomBuilder reflex m)
           => T.Text -> m a -> GridI n n n m a
blockCellS classes child = liftGridI $
  elClass "div" ("inline-block" <> " " <> widthClass @n <> " " <> classes) child

-- | Consume budget AND render an inline-block cell.
--
-- @
-- block \@(F 1 3) (text \"Left\")  -- consumes 1/3, renders as w-1/3
-- block \@(F 70 100) content      -- consumes 70/100, renders as w-[70%]
-- @
block :: forall (n :: (Nat, Nat)) (total :: (Nat, Nat)) (rem :: (Nat, Nat)) reflex m a.
         (RatLeq n rem, KnownRat n, DomBuilder reflex m, Monad m)
      => m a -> GridI total rem (RatSub rem n) m a
block = consume @n . blockCell @n

-- | Like 'block' but also applies ClasshSS styling to the div.
--
-- @
-- blockS \@(F 40 100) $(classh' [bgColor .~~ solidColor (Cyan C50), p .~~ TWSize 3]) $
--   text \"content\"
-- @
blockS :: forall (n :: (Nat, Nat)) (total :: (Nat, Nat)) (rem :: (Nat, Nat)) reflex m a.
          (RatLeq n rem, KnownRat n, DomBuilder reflex m, Monad m)
       => T.Text -> m a -> GridI total rem (RatSub rem n) m a
blockS classes = consume @n . blockCellS @n classes

-- | Run a row. Leftover budget is allowed.
--
-- Requires a 'ViewCtx' \'W token — obtain one from 'viewportW' or 'viewport'.
--
-- @
-- viewportW $ \\w ->
--   blockRow \@(N 100) w $
--     block \@(F 30 100) sidebar .>> block \@(F 70 100) main_
-- @
blockRow :: forall (total :: (Nat, Nat)) rem_out m a.
            ViewCtx 'W -> GridI total total rem_out m a -> m a
blockRow _ = runGridI

-- | Run a row, requiring ALL budget consumed.
-- Numerator of remaining must be 0.
--
-- @
-- viewportW $ \\w ->
--   blockRowExact \@(N 1) w $
--     block \@(F 1 3) a .>> block \@(F 2 3) b   -- 1/3 + 2/3 = 1
-- @
blockRowExact :: forall (total :: (Nat, Nat)) rem_out m a.
                 (RatNum rem_out ~ 0)
              => ViewCtx 'W -> GridI total total rem_out m a -> m a
blockRowExact _ = runGridI_exact


--------------------------------------------------------------------------------
-- Vertical layout (height-tracked, block display)
--------------------------------------------------------------------------------

-- | Wrap a widget in a div with height derived from
-- the type-level rational @n@.
--
-- No @inline-block@ — divs already stack vertically by default.
--
-- This does NOT consume budget — use 'vblock' for budget-tracked layout.
vblockCell :: forall (n :: (Nat, Nat)) reflex m a.
               (KnownRat n, DomBuilder reflex m)
            => m a -> GridI n n n m a
vblockCell child = liftGridI $
  elClass "div" (heightClass @n) child

-- | Like 'vblockCell' but also applies ClasshSS styling.
vblockCellS :: forall (n :: (Nat, Nat)) reflex m a.
                (KnownRat n, DomBuilder reflex m)
             => T.Text -> m a -> GridI n n n m a
vblockCellS classes child = liftGridI $
  elClass "div" (heightClass @n <> " " <> classes) child

-- | Consume height budget AND render a height-constrained div.
--
-- @
-- vblock \@(F 10 100) header   -- consumes 10/100, renders as h-[10%]
-- vblock \@(F 1 2) topHalf     -- consumes 1/2, renders as h-1/2
-- @
vblock :: forall (n :: (Nat, Nat)) (total :: (Nat, Nat)) (rem :: (Nat, Nat)) reflex m a.
          (RatLeq n rem, KnownRat n, DomBuilder reflex m, Monad m)
       => m a -> GridI total rem (RatSub rem n) m a
vblock = consume @n . vblockCell @n

-- | Like 'vblock' but also applies ClasshSS styling to the div.
--
-- @
-- vblockS \@(F 80 100) $(classh' [bgColor .~~ solidColor (Gray C50)]) $
--   text \"main content\"
-- @
vblockS :: forall (n :: (Nat, Nat)) (total :: (Nat, Nat)) (rem :: (Nat, Nat)) reflex m a.
           (RatLeq n rem, KnownRat n, DomBuilder reflex m, Monad m)
        => T.Text -> m a -> GridI total rem (RatSub rem n) m a
vblockS classes = consume @n . vblockCellS @n classes

-- | Run a vertical column. Leftover budget is allowed.
--
-- Requires a 'ViewCtx' \'H token — obtain one from 'viewportH' or 'viewport'.
--
-- @
-- viewportH $ \\h ->
--   vblockCol \@(N 100) h $
--     vblock \@(F 10 100) header .>> vblock \@(F 80 100) main_ .>> vblock \@(F 10 100) footer
-- @
vblockCol :: forall (total :: (Nat, Nat)) rem_out m a.
             ViewCtx 'H -> GridI total total rem_out m a -> m a
vblockCol _ = runGridI

-- | Run a vertical column requiring ALL height budget consumed.
-- Numerator of remaining must be 0.
vblockColExact :: forall (total :: (Nat, Nat)) rem_out m a.
                  (RatNum rem_out ~ 0)
               => ViewCtx 'H -> GridI total total rem_out m a -> m a
vblockColExact _ = runGridI_exact


--------------------------------------------------------------------------------
-- Internal
--------------------------------------------------------------------------------

-- | Compute the Tailwind width class for a type-level rational.
widthClass :: forall (n :: (Nat, Nat)). KnownRat n => T.Text
widthClass = "w-" <> showTW (fromRat (Proxy @n) :: TWSizeOrFraction)

-- | Compute the Tailwind height class for a type-level rational.
heightClass :: forall (n :: (Nat, Nat)). KnownRat n => T.Text
heightClass = "h-" <> showTW (fromRat (Proxy @n) :: TWSizeOrFraction)
