-- | Template slot substitution engine.
--
-- Provides @{{::=name}}@ placeholders that can be inserted into statically
-- rendered DOM via 'templateSlot', then replaced with actual content via
-- 'unTemplate'. The find-and-replace pass delegates to @scrappy-template@.
module Lamarckian.Template where

import Lamarckian.Types

import Reflex.Dom.Core
import Scrappy.Template (templateParserWith, unTemplateWith)
import Text.Parsec (Stream, ParsecT)

import Control.Monad.IO.Class
import Data.Bifunctor
import Crypto.Hash.SHA256 (hash)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BSC

-- | Render a 'StaticDom' widget to a 'String', then substitute all
-- @{{::=name}}@ placeholders using the provided map.
renderStaticTemplate :: Map.Map Name' String -> StaticDom () -> IO String
renderStaticTemplate config dom = do
  (_, html) <- renderStatic dom
  pure $ unTemplate config $ T.unpack . T.decodeUtf8 $ html

-- | Emit a @{{::=name}}@ placeholder into the DOM as a text node.
templateSlot :: DomBuilder t m => T.Text -> m ()
templateSlot name = text $ "{{::=" <> name <> "}}"

-- TODO: use template haskell and staticWhich to make this compile time so that it fails
-- if an undefined name does not exist in the Map and can also give warnings when some are not used

-- | Replace all @{{::=name}}@ occurrences in the input 'String' using the map.
--
-- Calls 'error' if a placeholder references a name not present in the map.
unTemplate :: Map.Map String String -> String -> String
unTemplate = unTemplateWith "{{::=" "}}"


type Name' = String

-- | Parsec parser matching @{{::=name}}@. Valid names: start with letter,
-- underscore, or hyphen; followed by alphanumeric, underscore, or hyphen.
templateParser :: Stream s m Char => ParsecT s u m Name'
templateParser = templateParserWith "{{::=" "}}"


-- | Read a file and wrap its content as a single 'TemplateVars' entry
-- under the given key.
readFileToTemplateKey' :: MonadIO m => String -> FilePath -> m TemplateVars
readFileToTemplateKey' key fp = do
  x <- liftIO $ readFile fp
  pure $ Map.fromList [ (key,  x)]

-- for now I'll include Template logic in here

slotKey :: String
slotKey = "slot"

-- | SHA256 hash a string, returning @\"h-\" <> hex(digest)@.
-- The @\"h-\"@ prefix ensures the result starts with a letter,
-- satisfying the 'templateParser' name constraints.
hashString :: String -> String
hashString input =
  let bsInput = BSC.pack input
      digest  = hash bsInput
  in "h-" <> (BSC.unpack $ B16.encode digest)
  -- We need this to only return letters and numbers for `streamEdit`
  -- and use of `validName` parser


-- | Build a @(SlotKey, HtmlString)@ pair. The 'SlotKey' is the SHA256 hash of the value.
mkTmplValue :: T.Text -> (SlotKey, HtmlString)
mkTmplValue val = (hashString $ T.unpack val, HtmlString val)

-- | Like 'mkTmplValue' but carries an extra annotation value.
mkTmplValueA :: T.Text -> a -> (SlotKey, HtmlString, a)
mkTmplValueA val x = (hashString $ T.unpack val, HtmlString val, x)

-- | Build a list of @(SlotKey, HtmlString)@ pairs, prefixing each key with the 'GroupKey'.
mkTmplValues :: GroupKey -> [T.Text] -> [(SlotKey, HtmlString)]
mkTmplValues gKey vals = first (gKey <>) . mkTmplValue <$> vals

-- | Like 'mkTmplValues' but each entry carries an annotation.
mkTmplValuesA :: GroupKey -> [(T.Text,a)] -> [(SlotKey, HtmlString, a)]
mkTmplValuesA _ [] = []
mkTmplValuesA gKey ((val,a):xs) = (gKey <> hashString (T.unpack val), HtmlString val, a) : mkTmplValuesA gKey xs
-- We want to allow for heterogenous inputs and also keep ordering
  -- mkSlotKey input1 : (mkOrderedSlotKeys inputs2) <> (mkOrderedSlotKeys inputs3)

-- | Extract the flat @SlotKey -> HtmlString@ map from 'HTemplateVars',
-- discarding annotations and group keys.
getHTemplateValues :: HTemplateVars k a -> HTemplateValues
getHTemplateValues = Map.fromList . fmap grab_1_2 . mconcat . Map.elems
  where
    grab_1_2 (_1, _2,_) = (_1,_2)
