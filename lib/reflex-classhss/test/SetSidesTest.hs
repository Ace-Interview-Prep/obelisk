{-# LANGUAGE OverloadedStrings #-}

module Main where

import Reflex.Dom.Core
import Classh
import Classh.Reflex.Layout

-- This test replicates the exact failing pattern from Engineers.hs:61
-- where `row [b .~~ TWSize 2] $ ...` fails with ambiguous type error

main :: IO ()
main = do
  putStrLn "Testing: row [b .~~ TWSize 2]"
  _ <- renderStatic $ row [b .~~ TWSize 2] $ text "test"
  putStrLn "Success! The issue is fixed!"
