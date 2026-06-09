module Jenga.Command.Hoogle
  ( HoogleOpts (..)
  , hoogleParser
  , run
  ) where

import Options.Applicative
import System.Which (staticWhich)

import Jenga.Command.Utils (execProcess)

hooglePath :: FilePath
hooglePath = $(staticWhich "hoogle")

data HoogleOpts = HoogleOpts
  { hooglePort :: Int
  }

hoogleParser :: Parser HoogleOpts
hoogleParser = HoogleOpts
  <$> option auto
      ( long "port" <> short 'p' <> metavar "PORT" <> value 8080
      <> help "Port to serve on (default: 8080)" )

run :: HoogleOpts -> IO ()
run opts =
  execProcess hooglePath ["server", "--local", "--port=" <> show (hooglePort opts)]
