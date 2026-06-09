{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Classh.Reflex.Eff
  ( gridCol
  , col
  , centerSimple
  ) where

import Classh
import qualified Data.Text as T
import Effectful (Eff)
import Reflex.Effectful.Effect.Dom (elClass)
import Reflex.Effectful.Run (DomEff')

gridCol :: DomEff' es t => ColInt -> Eff es a -> Eff es a
gridCol cInt ma = elClass "div" ("grid grid-cols-" <> showTW cInt) ma

col :: DomEff' es t => [Int] -> Eff es a -> Eff es a
col = responsiveRowCol'

centerSimple :: DomEff' es t => Eff es a -> Eff es a
centerSimple = elClass "div" $(classh' [ pos .~~ centered, w .~~ pct 100, h .~~ pct 100 ])

-- Effectful version of responsiveRowCol
responsiveRowCol' :: DomEff' es t => [Int] -> Eff es a -> Eff es a
responsiveRowCol' cInts = elClass "div" (zipScreensCols cInts)

zipScreensCols :: [Int] -> T.Text
zipScreensCols cInts' =
  let
    screens :: [T.Text]
    screens = ["", "sm:", "md:", "lg:", "xl:", "2xl:"]
    mkColClass scr n = scr <> "col-span-" <> T.pack (show n)
  in T.unwords $ zipWith mkColClass screens cInts'
