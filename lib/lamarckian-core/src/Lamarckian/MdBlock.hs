-- | Composable Markdown block abstraction.
--
-- 'MdBlock' wraps a function @(Text -> Text) -> Text@ where the argument
-- is a link-rewriting function applied at render time. This allows blocks
-- to be composed via 'Semigroup' while deferring link resolution.
--
-- Note: 'MdBlock' does not parse or render Markdown itself. The actual
-- MMark rendering lives in @Lamarckian.MMark@ in the @lamarckian@ package.
module Lamarckian.MdBlock where

import Data.Text

-- | A block of pre-rendered HTML that takes a local link resolver to
-- produce final 'Text'. Compose with '<>'.
newtype MdBlock = MdBlock { runMdBlock :: (Text -> Text) -> Text }

instance Semigroup MdBlock where
  md1 <> md2 =  MdBlock $ \mkLocalLink -> (runMdBlock md1) mkLocalLink <> (runMdBlock md2) mkLocalLink

-- | Render an 'MdBlock' by supplying a link-rewriting function.
renderMdBlock :: (Text -> Text) -> MdBlock -> Text
renderMdBlock mkLocalLink md = runMdBlock md mkLocalLink

