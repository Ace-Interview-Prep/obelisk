module Jenga.Command.Deploy
  ( DeployCommand (..)
  , deployParser
  , run
  ) where

import Options.Applicative

import qualified Jenga.Command.Deploy.Init as Init
import qualified Jenga.Command.Deploy.Push as Push

data DeployCommand
  = Deploy_Init Init.InitOpts
  | Deploy_Push Push.PushOpts

deployParser :: Parser DeployCommand
deployParser = subparser
  ( command "init" (mkInfo "Initialize a deployment staging directory"
      (Deploy_Init <$> Init.optsParser))
  <> command "push" (mkInfo "Build and deploy to configured hosts"
      (Deploy_Push <$> Push.optsParser))
  )
  where
    mkInfo desc p = info (helper <*> p) (progDesc desc)

run :: DeployCommand -> IO ()
run (Deploy_Init opts) = Init.run opts
run (Deploy_Push opts) = Push.run opts
