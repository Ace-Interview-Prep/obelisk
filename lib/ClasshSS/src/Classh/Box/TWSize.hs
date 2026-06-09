--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Box.TWSize
--  Copyright   :  (c) 2024, Galen Sprout
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Galen Sprout <galen.sprout@gmail.com>
--  Stability   :  provisional
--  Portability :  portable
--
--  'TWSize' and 'TWSizeOrFraction' are common patterns that exist among Tailwind
--  classes. There are a variety of uses such as width/height, padding and margin
--
--  As the names imply, TWSizeOrFraction is a superset containing the TWSize type
--
--  This also exposes a way to use CSS sizes, see 'CSSSize' and 'HasCSSSize' via
--  'TWSize'
--
--  Example use:
--
-- @
--  $(classh' [ padding . paddingT .~~ TWSize 8 ])
--  -- or with shorthand
--  $(classh' [ pt .~~ TWSize 8, w .~~ twSize' 8, mb .~~ pix 3 ])
-- @
--------------------------------------------------------------------------------



{-# LANGUAGE TypeFamilies #-}

module Classh.Box.TWSize
  ( module X
  , twSize'
  , intToDivInt
  , TWSize(..)
  , TWSizeOrFraction(..)
  ) where

import Classh.Class.HasCSSSize
import Classh.Class.ShowTW
import Classh.Class.IsCSS
import Classh.Internal.TShow
import Control.Monad.Consumable (Subtractive(..))

import Classh.Internal.CSSSize as X
import Classh.Box.DivInt as X

import Data.Default
import Data.Ratio (numerator, denominator)
import qualified Data.Text as T

-- | Use a TWSize where the config is expecting a TWSizeOrFraction
twSize' :: Float -> TWSizeOrFraction
twSize' = TWSize' . TWSize


-- | The float component of classes like padding or margin or sizing
data TWSize
  = TWSize Float
  -- ^ TWSize x == "somePrefix-x"
  | TWSize_Custom CSSSize
  -- ^ Example output: pt-[3px] 
  deriving Show

instance ShowTW TWSize where
  showTW = \case
    TWSize float ->
      if fromIntegral (truncate float :: Int) == float
      then tshow $ (truncate float :: Int)
      else tshow float
    TWSize_Custom c -> "[" <> renderCSS c <> "]"

-- | https://tailwindcss.com/docs/width
-- | https://tailwindcss.com/docs/height
-- | etc
data TWSizeOrFraction
  = TWSize' TWSize
  -- ^ see TWSize 
  | TWFraction Int DivInt
  -- ^ Eg. w-11/12
  | TWSize_Full
  -- ^ == (h|w)-full
  | TWSize_Screen
  -- ^ (h|w)-screen
  | TWSize_Min
  -- ^ (h|w)-min
  | TWSize_Max
  -- ^ (h|w)-max 
  | TWSize_Fit
  -- ^ (h|w)-fit
  | TWSize_Auto
  -- ^ (h|w)-auto
  deriving Show

instance ShowTW TWSizeOrFraction where
  showTW = \case
    TWSize' s -> showTW s
    TWFraction n d -> tshow n <> "/" <> showTW d
    class' -> T.toLower . T.drop 7 . tshow $ class'

-- | > == TWSize_Auto
instance Default TWSizeOrFraction where
  def = TWSize_Auto

instance HasCSSSize TWSize where
  pix = TWSize_Custom . Pixel
  pct = TWSize_Custom . Percent
  vh = TWSize_Custom . Vh
  vw = TWSize_Custom . Vw
  rem = TWSize_Custom . Rem

instance HasCSSSize TWSizeOrFraction where
  pix = TWSize' . TWSize_Custom . Pixel --px
  pct = TWSize' . TWSize_Custom . Percent --pct
  vh = TWSize' . TWSize_Custom . Vh --vh
  vw = TWSize' . TWSize_Custom . Vw--vw
  rem = TWSize' . TWSize_Custom . Rem -- Classh.rem

-- | Convert to a raw Float for cross-kind subtraction.
-- Returns Nothing only for keywords that have no numeric interpretation.
toFloat :: TWSizeOrFraction -> Maybe Float
toFloat (TWSize' (TWSize f))                = Just f
toFloat (TWSize' (TWSize_Custom (Pixel n))) = Just (fromIntegral n)
toFloat (TWSize' (TWSize_Custom (Percent n))) = Just (fromIntegral n)
toFloat (TWSize' (TWSize_Custom (Vh n)))    = Just (fromIntegral n)
toFloat (TWSize' (TWSize_Custom (Vw n)))    = Just (fromIntegral n)
toFloat (TWSize' (TWSize_Custom (Rem f)))   = Just f
toFloat (TWFraction n d)                    = Just (fromIntegral n / divIntToFloat d)
toFloat _                                   = Nothing

divIntToFloat :: DivInt -> Float
divIntToFloat D2  = 2
divIntToFloat D3  = 3
divIntToFloat D4  = 4
divIntToFloat D5  = 5
divIntToFloat D6  = 6
divIntToFloat D12 = 12

instance Subtractive TWSizeOrFraction where
  type Difference TWSizeOrFraction = Maybe TWSizeOrFraction
  -- TWSize Float: preserve structure
  TWSize' (TWSize a) .- TWSize' (TWSize b)
    | b > a     = Nothing
    | otherwise = Just (TWSize' (TWSize (a - b)))
  -- Same CSSSize unit: preserve unit
  TWSize' (TWSize_Custom ca) .- TWSize' (TWSize_Custom cb) =
    case cssSub ca cb of
      Just r  -> Just (TWSize' (TWSize_Custom r))
      Nothing -> floatSub (TWSize' (TWSize_Custom ca)) (TWSize' (TWSize_Custom cb))
    where
      cssSub (Pixel a)   (Pixel b)   = intSub Pixel a b
      cssSub (Percent a) (Percent b) = intSub Percent a b
      cssSub (Vh a)      (Vh b)      = intSub Vh a b
      cssSub (Vw a)      (Vw b)      = intSub Vw a b
      cssSub (Rem a)     (Rem b)
        | b > a     = Nothing
        | otherwise = Just (Rem (a - b))
      cssSub _ _   = Nothing
      intSub con a b
        | b > a     = Nothing
        | otherwise = Just (con (a - b))
  -- Same-denominator fractions: preserve structure
  TWFraction a da .- TWFraction b db
    | da == db && b > a = Nothing
    | da == db          = Just (TWFraction (a - b) da)
    | otherwise         = floatSub (TWFraction a da) (TWFraction b db)
  -- Cross-kind: convert both to Float, subtract
  a .- b = floatSub a b

-- | Cross-kind subtraction via Float. Nothing only on underflow or
-- if either side is a keyword with no numeric value.
floatSub :: TWSizeOrFraction -> TWSizeOrFraction -> Maybe TWSizeOrFraction
floatSub a b = case (toFloat a, toFloat b) of
  (Just fa, Just fb)
    | fb > fa   -> Nothing
    | otherwise -> Just (TWSize' (TWSize (fa - fb)))
  _ -> Nothing

-- | Map an integer denominator to a DivInt, if it matches a Tailwind fraction.
intToDivInt :: Integer -> Maybe DivInt
intToDivInt 2  = Just D2
intToDivInt 3  = Just D3
intToDivInt 4  = Just D4
intToDivInt 5  = Just D5
intToDivInt 6  = Just D6
intToDivInt 12 = Just D12
intToDivInt _  = Nothing

instance Num TWSizeOrFraction where
  fromInteger = TWSize' . TWSize . fromInteger

  a + b = case (toFloat a, toFloat b) of
    (Just fa, Just fb) -> TWSize' (TWSize (fa + fb))
    _ -> error "Num TWSizeOrFraction: (+) undefined on keyword values (Full, Screen, etc.)"

  a - b = case (toFloat a, toFloat b) of
    (Just fa, Just fb) -> TWSize' (TWSize (fa - fb))
    _ -> error "Num TWSizeOrFraction: (-) undefined on keyword values (Full, Screen, etc.)"

  a * b = case (toFloat a, toFloat b) of
    (Just fa, Just fb) -> TWSize' (TWSize (fa * fb))
    _ -> error "Num TWSizeOrFraction: (*) undefined on keyword values (Full, Screen, etc.)"

  negate a = case toFloat a of
    Just fa -> TWSize' (TWSize (negate fa))
    Nothing -> error "Num TWSizeOrFraction: negate undefined on keyword values"

  abs a = case toFloat a of
    Just fa -> TWSize' (TWSize (abs fa))
    Nothing -> error "Num TWSizeOrFraction: abs undefined on keyword values"

  signum a = case toFloat a of
    Just fa -> TWSize' (TWSize (signum fa))
    Nothing -> error "Num TWSizeOrFraction: signum undefined on keyword values"

instance Fractional TWSizeOrFraction where
  fromRational r
    | d == 1    = TWSize' (TWSize (fromInteger n))
    | Just di <- intToDivInt d = TWFraction (fromIntegral n) di
    | otherwise = TWSize' (TWSize (fromRational r))
    where
      n = numerator r
      d = denominator r

  a / b = case (toFloat a, toFloat b) of
    (Just fa, Just fb) -> TWSize' (TWSize (fa / fb))
    _ -> error "Fractional TWSizeOrFraction: (/) undefined on keyword values"
