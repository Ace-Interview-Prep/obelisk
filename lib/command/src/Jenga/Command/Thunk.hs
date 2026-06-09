module Jenga.Command.Thunk
  ( run
  ) where

import System.Which (staticWhich)

import Jenga.Command.Utils (execProcess)

nixThunkPath :: FilePath
nixThunkPath = $(staticWhich "nix-thunk")

-- | jenga thunk delegates entirely to nix-thunk, passing all args through.
run :: [String] -> IO ()
run args = execProcess nixThunkPath args
