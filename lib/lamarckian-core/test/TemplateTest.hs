{-# LANGUAGE OverloadedStrings #-}
module TemplateTest (spec) where

import Test.Hspec
import Control.Exception (evaluate)

import Lamarckian.Template
import Lamarckian.Types

import qualified Data.Map as Map
import qualified Data.Text as T

spec :: Spec
spec = describe "Lamarckian.Template" $ do

  describe "unTemplate" $ do
    it "substitutes a single placeholder" $ do
      let m = Map.fromList [("name", "world")]
      unTemplate m "hello {{::=name}}!" `shouldBe` "hello world!"

    it "substitutes multiple distinct placeholders" $ do
      let m = Map.fromList [("a", "X"), ("b", "Y")]
      unTemplate m "{{::=a}} and {{::=b}}" `shouldBe` "X and Y"

    it "substitutes the same placeholder appearing twice" $ do
      let m = Map.fromList [("x", "Z")]
      unTemplate m "{{::=x}} then {{::=x}}" `shouldBe` "Z then Z"

    it "returns input unchanged when no placeholders exist" $ do
      let m = Map.fromList [("unused", "val")]
      unTemplate m "no slots here" `shouldBe` "no slots here"

    it "errors on a missing key" $ do
      let m = Map.empty :: Map.Map String String
      evaluate (unTemplate m "{{::=missing}}") `shouldThrow` anyErrorCall

  describe "hashString" $ do
    it "starts with h-" $ do
      take 2 (hashString "anything") `shouldBe` "h-"

    it "is deterministic" $ do
      hashString "test" `shouldBe` hashString "test"

    it "produces different output for different input" $ do
      hashString "a" `shouldNotBe` hashString "b"

  describe "mkTmplValue" $ do
    it "creates a SlotKey starting with h-" $ do
      let (key, _) = mkTmplValue "hello"
      take 2 key `shouldBe` "h-"

    it "preserves the input text in HtmlString" $ do
      let (_, HtmlString val) = mkTmplValue "hello"
      val `shouldBe` "hello"

  describe "mkTmplValueA" $ do
    it "carries the annotation through" $ do
      let (_, _, ann) = mkTmplValueA "hello" (42 :: Int)
      ann `shouldBe` 42

  describe "mkTmplValues" $ do
    it "prefixes each SlotKey with the GroupKey" $ do
      let results = mkTmplValues "grp" ["a", "b"]
      all (\(k, _) -> take 3 k == "grp") results `shouldBe` True

    it "preserves the number of entries" $ do
      let results = mkTmplValues "g" ["x", "y", "z"]
      length results `shouldBe` 3

  describe "mkTmplValuesA" $ do
    it "preserves annotations in order" $ do
      let results = mkTmplValuesA "g" [("a", 1 :: Int), ("b", 2)]
      map (\(_, _, a) -> a) results `shouldBe` [1, 2]

    it "returns empty for empty input" $ do
      let results = mkTmplValuesA "g" ([] :: [(T.Text, Int)])
      length results `shouldBe` 0

  describe "getHTemplateValues" $ do
    it "extracts SlotKey -> HtmlString map discarding annotations" $ do
      let key = hashString "val"
          vars = Map.fromList [(1 :: Int, [(key, HtmlString "val", True)])]
          result = getHTemplateValues vars
      Map.size result `shouldBe` 1
      fmap getHtml (Map.lookup key result) `shouldBe` Just "val"
