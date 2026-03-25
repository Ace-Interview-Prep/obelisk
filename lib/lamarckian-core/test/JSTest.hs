{-# LANGUAGE OverloadedStrings #-}
module JSTest (spec) where

import Test.Hspec
import Data.List (isInfixOf)

import Lamarckian.JS

spec :: Spec
spec = describe "Lamarckian.JS" $ do

  describe "Script" $ do
    it "mempty is empty" $ do
      let Script s = mempty
      s `shouldBe` ""

    it "semigroup concatenates" $ do
      let Script s = Script "a();" <> Script "b();"
      s `shouldBe` "a();b();"

  describe "js2" $ do
    it "contains the function name" $ do
      let JSFunc s = js2 "myFunc" (1 :: Int) (2 :: Int)
      "myFunc" `isInfixOf` s `shouldBe` True

    it "contains both arguments" $ do
      let JSFunc s = js2 "f" (10 :: Int) (20 :: Int)
      "10" `isInfixOf` s `shouldBe` True
      "20" `isInfixOf` s `shouldBe` True

  describe "js3" $ do
    it "contains all three arguments" $ do
      let JSFunc s = js3 "g" (1 :: Int) (2 :: Int) (3 :: Int)
      "1" `isInfixOf` s `shouldBe` True
      "2" `isInfixOf` s `shouldBe` True
      "3" `isInfixOf` s `shouldBe` True
