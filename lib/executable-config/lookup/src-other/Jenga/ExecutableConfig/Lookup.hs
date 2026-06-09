{-# LANGUAGE OverloadedStrings #-}
module Jenga.ExecutableConfig.Lookup where

import Data.Map (Map)
import Data.Text (Text)
import Data.ByteString (ByteString)

import Jenga.Configs.Internal.Directory (getConfigsFromDirectory)

getConfigs :: IO (Map Text ByteString)
getConfigs = getConfigsFromDirectory "config"
