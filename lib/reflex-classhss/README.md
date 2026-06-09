# reflex-classh

Reflex.Dom convenience functions for ClasshSS. Provides text rendering, element wrappers, and grid layout primitives.

## Overview

While ClasshSS generates class strings (library-agnostic), **reflex-classh** provides Reflex.Dom-specific utilities:

- `textS`, `textPosition` - Text rendering with ClasshSS styling
- `elTW`, `elDynTW` - Type-safe element wrappers using BoxConfig
- `row`, `col`, `gridCol` - Grid-based responsive layout system
- `centerSimple`, `centerHSimple` - Positioning utilities

## Installation

```cabal
build-depends:
    reflex-classhss
  , ClasshSS
  , reflex-dom-core
```

## Quick Example

```haskell
{-# LANGUAGE TemplateHaskell #-}

import Classh
import Classh.Reflex
import Reflex.Dom.Core

-- Responsive three-column layout
myPage :: DomBuilder t m => m ()
myPage =
  row [y .~~ TWSize 10] $
    gridCol Col12 $ do
      col [12, 12, 4] $ textS $(classhText [text_size .~~ XL]) "Column 1"
      col [12, 12, 4] $ textS $(classhText [text_size .~~ XL]) "Column 2"
      col [12, 12, 4] $ textS $(classhText [text_size .~~ XL]) "Column 3"
```

**Breakdown:**
- `row [y .~~ TWSize 10]` - Creates row with vertical spacing
- `gridCol Col12` - 12-column grid container
- `col [12, 12, 4]` - Responsive spans: Mobile 12, SM 12, MD 4

## Core Functions

### Text Rendering

```haskell
-- Styled text (wraps in <span>)
textS :: CompiledS -> Text -> m ()
textS $(classhText [text_color .~~ Blue C500, text_size .~~ XL]) "Hello"

-- Text positioning (wraps in <div>)
textPosition :: CompiledS -> m a -> m a
textPosition $(classhTextPos [textAlign .~~ TextCenter]) $ text "Centered"

-- Dynamic variants
textDynS :: Dynamic t CompiledS -> Text -> m ()
dynTextS :: CompiledS -> Dynamic t Text -> m ()
```

### Element Wrappers

```haskell
-- Like elClass but takes BoxConfig
elTW :: Text -> BoxConfig -> m a -> m a
elTW "div" (def & bgColor .~~ Blue C500 & p .~~ TWSize 4) $ content

-- Dynamic BoxConfig
elDynTW :: Text -> Dynamic t BoxConfig -> m a -> m a

-- Template Haskell shortcuts
$(divClassh' [bgColor .~~ White, p .~~ TWSize 6]) $ content
$(textClassh' [text_color .~~ Gray C900]) "Text"
```

### Grid Layout

```haskell
-- Row separator
row :: [BoxPadding -> BoxPadding] -> m a -> m a
row [y .~~ TWSize 10] $ content

-- N-column grid
gridCol :: ColInt -> m a -> m a
gridCol Col12 $ do
  col [6] $ text "Left"
  col [6] $ text "Right"

-- Responsive columns (mobile-first)
col :: [Int] -> m a -> m a
col [12, 12, 6, 4] $ content  -- Mobile: 12, SM: 12, MD: 6, LG: 4

-- Column with start position
colFrom :: [(Int, Int)] -> m a -> m a
colFrom [(2, 4)] $ content  -- Start col 2, span 4

-- Dynamic columns (e.g., collapsible sidebar)
colDyn :: Dynamic t [Int] -> m a -> m a
```

### Positioning

```haskell
-- Center both axes
centerSimple :: m a -> m a

-- Center horizontally
centerHSimple :: m a -> m a

-- Center with responsive width
responsiveXPaddedRegion :: [TWSizeOrFraction] -> m a -> m a
responsiveXPaddedRegion [pct 100, pct 80, pct 60] $ content
```

## Common Patterns

### Card Component

```haskell
card :: DomBuilder t m => m ()
card = elTW "div" cardStyle $ do
  textS $(classhText [text_size .~~ XL2, text_weight .~~ Bold]) "Title"
  textS $(classhText [text_color .~~ Gray C600]) "Description"
  where
    cardStyle = def
      & bgColor .~~ White
      & p .~~ TWSize 6
      & br .~~ R_Lg
      & shadow .~~ Shadow_Md
```

### Responsive Grid

```haskell
featureGrid :: DomBuilder t m => m ()
featureGrid =
  gridCol Col12 $ do
    col [12, 12, 4] $ feature "Fast"
    col [12, 12, 4] $ feature "Safe"
    col [12, 12, 4] $ feature "Easy"
```

### Centered Form

```haskell
loginForm :: DomBuilder t m => m ()
loginForm =
  centerSimple $
    responsiveXPaddedRegion [pct 100, pct 60, pct 40] $
      elTW "form" (def & bgColor .~~ White & p .~~ TWSize 8) $ do
        textS $(classhText [text_size .~~ XL2]) "Login"
        -- Form fields...
```

## Comparison

**Without reflex-classh:**
```haskell
elClass "div" $(classh' [position .~~ centered, w .~~ pct 100]) $
  elClass "span" $(classhText [text_color .~~ Blue C500]) $
    text "Hello"
```

**With reflex-classh:**
```haskell
centerHSimple $
  textS $(classhText [text_color .~~ Blue C500]) "Hello"
```

## Modules

- `Classh.Reflex.Text` - Text rendering (textS, paragraphs, intercalate)
- `Classh.Reflex.El` - Element wrappers (elTW, divClassh')
- `Classh.Reflex.Layout` - Grid layouts (row, col, gridCol)
- `Classh.Reflex.Place` - Positioning (centerSimple, responsiveXPaddedRegion)

## API Documentation

Run `cabal haddock` or see Haddock in source files.

## License

BSD-style
