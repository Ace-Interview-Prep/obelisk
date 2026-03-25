{-# LANGUAGE OverloadedStrings #-}
module MdBlockTest (spec) where

import Test.Hspec
import Lamarckian.MdBlock
import Data.Text (Text)

spec :: Spec
spec = describe "Lamarckian.MdBlock" $ do

  describe "renderMdBlock" $ do
    it "applies the link rewriter and returns text" $ do
      let block = MdBlock $ \rewrite -> "go to " <> rewrite "page"
      renderMdBlock (\l -> "/blog/" <> l) block `shouldBe` "go to /blog/page"

    it "passes identity rewriter through unchanged" $ do
      let block = MdBlock $ \rewrite -> "link: " <> rewrite "x"
      renderMdBlock id block `shouldBe` "link: x"

  describe "Semigroup" $ do
    it "concatenates two blocks" $ do
      let b1 = MdBlock $ \_ -> "hello "
          b2 = MdBlock $ \_ -> "world"
      renderMdBlock id (b1 <> b2) `shouldBe` "hello world"

    it "threads the same link rewriter to both blocks" $ do
      let b1 = MdBlock $ \r -> r "a"
          b2 = MdBlock $ \r -> r "b"
          rewriter :: Text -> Text
          rewriter x = "[" <> x <> "]"
      renderMdBlock rewriter (b1 <> b2) `shouldBe` "[a][b]"
