module Jenga.Command.Utils
  ( execProcess
  , runProcess
  , readProcess_
  , runProcessWithEnv
  , requireConfig
  , readConfigOr
  , strip
  , info
  , die
  , devGhcOptions
  , devCrossCabalArgs
  ) where

import Data.Char (isSpace)
import System.Directory (doesFileExist)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..), exitFailure)
import System.IO (hPutStrLn, stderr)
import System.Posix.Process (executeFile)
import System.Process (CreateProcess (..), createProcess, readCreateProcessWithExitCode, proc, waitForProcess)

-- | Replace the current process (unix exec). Does not return on success.
execProcess :: FilePath -> [String] -> IO a
execProcess path args = do
  executeFile path False args Nothing
  -- executeFile only returns on error
  die $ "exec failed: " <> path

-- | Run a process and wait for it to exit. Dies on failure.
runProcess :: FilePath -> [String] -> IO ()
runProcess path args = do
  (ec, _, err) <- readCreateProcessWithExitCode (proc path args) ""
  case ec of
    ExitSuccess -> pure ()
    ExitFailure c -> die $ path <> " exited with code " <> show c
      <> if null err then "" else ": " <> err

-- | Run a process and capture stdout. Dies on failure.
readProcess_ :: FilePath -> [String] -> IO String
readProcess_ path args = do
  (ec, out, err) <- readCreateProcessWithExitCode (proc path args) ""
  case ec of
    ExitSuccess -> pure out
    ExitFailure c -> die $ path <> " exited with code " <> show c
      <> if null err then "" else ": " <> err

-- | Print an informational message to stderr.
info :: String -> IO ()
info msg = hPutStrLn stderr $ "==> " <> msg

-- | Print an error message and exit.
die :: String -> IO a
die msg = do
  hPutStrLn stderr $ "error: " <> msg
  exitFailure

-- | Run a process with extra environment variables merged into the current env.
runProcessWithEnv :: FilePath -> [String] -> [(String, String)] -> IO ()
runProcessWithEnv path args envVars = do
  currentEnv <- getEnvironment
  let mergedEnv = envVars <> filter (\(k, _) -> k `notElem` map fst envVars) currentEnv
  let cp = (proc path args)
        { env = Just mergedEnv
        , delegate_ctlc = True
        }
  (_, _, _, ph) <- createProcess cp
  ec <- waitForProcess ph
  case ec of
    ExitSuccess -> pure ()
    ExitFailure c -> die $ path <> " exited with code " <> show c

-- | GHC options for fast dev rebuilds (shared by jenga run and jenga repl).
devGhcOptions :: [String]
devGhcOptions =
  [ "--ghc-options=-O0"
  , "--ghc-options=-fno-specialise"
  , "--ghc-options=-fno-expose-all-unfoldings"
  , "--ghc-options=-fno-specialise-aggressively"
  , "--ghc-options=-fno-late-specialise"
  , "--ghc-options=-fno-cross-module-specialise"
  ]

-- | JENGA_CROSS_CABAL_ARGS value for dev builds.
devCrossCabalArgs :: String
devCrossCabalArgs = "-O0 " <> unwords devGhcOptions

-- | Read a required config file or die.
requireConfig :: FilePath -> String -> IO String
requireConfig path desc = do
  exists <- doesFileExist path
  if not exists
    then die $ "missing required config: " <> path <> " (" <> desc <> ")"
    else do
      val <- strip <$> readFile path
      if null val
        then die $ "empty required config: " <> path <> " (" <> desc <> ")"
        else pure val

-- | Read a config file, returning a default if it doesn't exist.
readConfigOr :: FilePath -> String -> IO String
readConfigOr path def = do
  exists <- doesFileExist path
  if exists then strip <$> readFile path else pure def

-- | Strip leading and trailing whitespace.
strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace
