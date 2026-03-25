{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec

import qualified TemplateTest
import qualified MdBlockTest
import qualified JSTest

main :: IO ()
main = hspec $ do
  TemplateTest.spec
  MdBlockTest.spec
  JSTest.spec
