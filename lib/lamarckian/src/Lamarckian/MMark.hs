{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Lamarckian.MMark where


import Text.IStr
import Lucid.Base (renderText)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Text.MMark as MMark
import Text.MMark.Extension.GhcSyntaxHighlighter
import Text.MMark.Extension.LinkTarget
import Text.MMark.Extension.Skylighting
import qualified Text.Megaparsec as MP
import Data.Text as T
import Data.Text.Lazy as LT

extensions :: [MMark.Extension]
extensions =
  [ ghcSyntaxHighlighter
  , linkTarget
  , skylighting
  ]


immark :: QuasiQuoter
immark = QuasiQuoter
  { quoteExp  = \md -> do
      [|
        case MMark.parse "markdown file" $ T.pack $(rstrExp md) of
          Left (e) -> error $ MP.errorBundlePretty e
          Right bundle -> LT.toStrict . renderText $ MMark.render $ useExtensions extensions bundle
       |]
  , quotePat  = error "markdown cannot be used as a pattern"
  , quoteType = error "markdown cannot be used as a type"
  , quoteDec  = error "markdown cannot be used as a declaration"
  }

      -- >> [| "" |]

      -- [| LT.toStrict
      --                    $ renderText
      --                    $ MMark.render
      --                    $ useExtensions
      --                    [ commentParagraph "-- |"
      --                    , fontAwesome
      --                    , footnotes
      --                    , ghcSyntaxHighlighter
      --                    , kbd
      --                    , linkTarget
      --                    , mathJax $ Just 'x'
      --                    --, obfuscateEmail
      --                    , punctuationPrettifier
      --                    , skylighting
      --                    --, toc
      --                    ]
      --                    $ either (\e -> error $ "invalid markdown: " <> show e) id
      --                    $ MMark.parse "filename"-- md
      --                    --   $ streamEdit matchPat (runHaskellExpr) md
      --                    $ T.pack
      --                    $(rstrExp md)

      --                   |]
    -- quoteExp  = \md -> [| LT.toStrict
    --                      $ renderText
    --                      $ MMark.render
    --                      $ useExtensions
    --                      [ commentParagraph "-- |"
    --                      , fontAwesome
    --                      , footnotes
    --                      , ghcSyntaxHighlighter
    --                      , kbd
    --                      , linkTarget
    --                      , mathJax $ Just 'x'
    --                      --, obfuscateEmail
    --                      , punctuationPrettifier
    --                      , skylighting
    --                      --, toc
    --                      ]
    --                      $ either (\e -> error $ "invalid markdown: " <> show e) id
    --                      $ MMark.parse "filename"-- md
    --                      --   $ streamEdit matchPat (runHaskellExpr) md
    --                      $ T.pack
    --                      $(rstrExp md)

    --                     |]



-- main :: IO ()
-- main = do
--   putStrLn $ [markdown|
-- # Hello World
-- This is a **Markdown** example with _quasi-quotes_.

-- - List item 1
-- - List item 2
--   |]
