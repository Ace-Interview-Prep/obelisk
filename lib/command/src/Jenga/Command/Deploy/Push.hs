module Jenga.Command.Deploy.Push
  ( PushOpts (..)
  , optsParser
  , run
  ) where

import Control.Monad (forM_, when)
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)
import Options.Applicative hiding (info)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))
import System.Which (staticWhich)

import qualified Data.ByteString.Char8 as BS

import Jenga.Command.Utils (die, info, readConfigOr, readProcess_,
                              requireConfig, runProcess, strip)

gitPath, nixBuildPath, sshPath, rsyncPath :: FilePath
gitPath      = $(staticWhich "git")
nixBuildPath = $(staticWhich "nix-build")
sshPath      = $(staticWhich "ssh")
rsyncPath    = $(staticWhich "rsync")

newtype PushOpts = PushOpts { pushDir :: String }

optsParser :: Parser PushOpts
optsParser = PushOpts
  <$> strArgument (metavar "DIR" <> help "Deployment staging directory")

-- | All config values read from the staging directory.
data DeployConfig = DeployConfig
  { cfgRouteHost      :: String
  , cfgAdminEmail     :: String
  , cfgEnableHttps    :: String
  , cfgFrontendTarget :: String
  , cfgInternalPort   :: String
  , cfgBaseUrl        :: String
  , cfgBackendArgs    :: String
  , cfgRedirectHosts  :: String
  , cfgProjectSource  :: String
  , cfgProjectBranch  :: String
  }

run :: PushOpts -> IO ()
run opts = do
  let dir = pushDir opts
  validateDir dir
  cfg   <- readDeployConfig dir
  hosts <- readHosts dir
  sshOpts <- buildSshOpts (dir </> "secrets" </> "ssh_key")
  revArgs <- resolveSource cfg
  systemPath <- buildClosure dir cfg revArgs
  forM_ hosts $ \host -> deployToHost dir sshOpts systemPath host
  info "Deployment complete."

--------------------------------------------------------------------------------
-- Steps
--------------------------------------------------------------------------------

validateDir :: FilePath -> IO ()
validateDir dir = do
  doesDirectoryExist dir >>= \e -> when (not e) $ die $ "not a directory: " <> dir
  doesFileExist (dir </> "deploy.nix") >>= \e -> when (not e) $
    die $ "not a deployment directory (missing deploy.nix): " <> dir

readDeployConfig :: FilePath -> IO DeployConfig
readDeployConfig dir = do
  routeHost      <- requireConfig (dir </> "config" </> "route_host") "domain name"
  adminEmail     <- requireConfig (dir </> "config" </> "admin_email") "ACME admin email"
  enableHttps    <- readConfigOr  (dir </> "config" </> "enable_https") "true"
  frontendTarget <- readConfigOr  (dir </> "config" </> "frontend_target") "wasm"
  internalPort   <- readConfigOr  (dir </> "config" </> "internal_port") "8000"
  baseUrl        <- readConfigOr  (dir </> "config" </> "base_url") "/"
  backendArgs    <- readConfigOr  (dir </> "config" </> "backend_args") ("--port=" <> internalPort)
  redirectHosts  <- readConfigOr  (dir </> "config" </> "redirect_hosts") ""
  projectSource  <- requireConfig (dir </> "project_source") "project git URL or path"
  projectBranch  <- readConfigOr  (dir </> "project_branch") "main"
  when (routeHost == "CHANGEME.example.com") $
    die $ "route_host is still the placeholder value. Edit " <> dir </> "config/route_host"
  pure DeployConfig
    { cfgRouteHost = routeHost, cfgAdminEmail = adminEmail
    , cfgEnableHttps = enableHttps, cfgFrontendTarget = frontendTarget
    , cfgInternalPort = internalPort, cfgBaseUrl = baseUrl
    , cfgBackendArgs = backendArgs, cfgRedirectHosts = redirectHosts
    , cfgProjectSource = projectSource, cfgProjectBranch = projectBranch
    }

readHosts :: FilePath -> IO [String]
readHosts dir = do
  content <- readFile (dir </> "hosts")
  let hosts = parseHosts content
  when (null hosts) $ die $ "no hosts configured in " <> dir </> "hosts"
  pure hosts

-- | Resolve the project source to a git rev (for remote sources).
-- Returns extra nix-build --argstr args.
resolveSource :: DeployConfig -> IO [String]
resolveSource cfg
  | "/" `isPrefixOf` cfgProjectSource cfg = do
      info $ "Project source: local path " <> cfgProjectSource cfg
      doesDirectoryExist (cfgProjectSource cfg) >>= \e -> when (not e) $
        die $ "project path does not exist: " <> cfgProjectSource cfg
      pure []
  | otherwise = do
      let branch = cfgProjectBranch cfg
          source = cfgProjectSource cfg
      info $ "Resolving " <> branch <> " on " <> source <> "..."
      lsOut <- readProcess_ gitPath ["ls-remote", source, "refs/heads/" <> branch]
      let rev = takeWhile (not . isSpace) lsOut
      when (null rev) $
        die $ "could not resolve branch '" <> branch <> "' on '" <> source <> "'"
      info $ "Resolved to: " <> rev
      pure ["--argstr", "projectRev", rev]

-- | Build the NixOS system closure via nix-build.
buildClosure :: FilePath -> DeployConfig -> [String] -> IO String
buildClosure dir cfg revArgs = do
  configHash <- strip <$> readProcess_ "sh"
    ["-c", "find " <> dir </> "config -type f -exec sha256sum {} + 2>/dev/null | sort | sha256sum | cut -d' ' -f1"]
  info "Building NixOS system closure..."
  systemPath <- strip <$> readProcess_ nixBuildPath
    ([ dir </> "deploy.nix"
     , "--argstr", "projectSource", cfgProjectSource cfg
     ] <> revArgs <>
     [ "--argstr", "routeHost",      cfgRouteHost cfg
     , "--argstr", "adminEmail",     cfgAdminEmail cfg
     , "--argstr", "enableHttps",    cfgEnableHttps cfg
     , "--argstr", "frontendTarget", cfgFrontendTarget cfg
     , "--argstr", "internalPort",   cfgInternalPort cfg
     , "--argstr", "baseUrl",        cfgBaseUrl cfg
     , "--argstr", "backendArgs",    cfgBackendArgs cfg
     , "--argstr", "redirectHosts",  cfgRedirectHosts cfg
     , "--argstr", "configHash",     configHash
     , "-A", "config.system.build.toplevel"
     , "--no-out-link"
     ])
  info $ "System closure: " <> systemPath
  pure systemPath

-- | Deploy a built system closure to a single host.
deployToHost :: FilePath -> [String] -> String -> String -> IO ()
deployToHost dir sshOpts systemPath rawHost = do
  let host = if '@' `elem` rawHost then rawHost else "root@" <> rawHost
  info $ "Deploying to " <> host <> "..."

  info "  Verifying SSH..."
  runProcess sshPath (sshOpts <> [host, "true"])

  info "  Copying system closure..."
  runProcess "nix-copy-closure" ["--to", host, "--use-substitutes", "--gzip", systemPath]

  info "  Syncing config..."
  runProcess rsyncPath
    [ "-e", unwords (sshPath : sshOpts)
    , "--chown=backend:backend", "-qarvz", "--delete"
    , dir </> "config" </> "backend/"
    , dir </> "config" </> "common/"
    , dir </> "config" </> "frontend/"
    , host <> ":/var/lib/backend/config/"
    ]

  info "  Activating..."
  runProcess sshPath $ sshOpts <>
    [ host, "--", "bash", "-c", activateScript systemPath ]

  info $ "  Done: " <> host

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

parseHosts :: String -> [String]
parseHosts = mapMaybe parseLine . lines
  where
    parseLine l =
      let stripped = strip (takeWhile (/= '#') l)
      in if null stripped then Nothing else Just stripped

buildSshOpts :: FilePath -> IO [String]
buildSshOpts keyPath = do
  let baseOpts = ["-o", "StrictHostKeyChecking=accept-new", "-o", "BatchMode=yes"]
  exists <- doesFileExist keyPath
  if not exists then pure baseOpts
  else do
    content <- BS.readFile keyPath
    if BS.null content then pure baseOpts
    else pure $ baseOpts <> ["-o", "IdentityFile=" <> keyPath, "-o", "IdentitiesOnly=yes"]

activateScript :: String -> String
activateScript systemPath = unwords
  [ "set -euo pipefail;"
  , "nix-env -p /nix/var/nix/profiles/system --set " <> q systemPath <> ";"
  , "CUR=$(readlink /run/booted-system/kernel 2>/dev/null || true);"
  , "NEW=$(readlink " <> q systemPath <> "/kernel 2>/dev/null || true);"
  , "if [ -n \"$CUR\" ] && [ -n \"$NEW\" ] && [ \"$CUR\" != \"$NEW\" ]; then"
  , "  echo '==> Kernel changed, scheduling reboot...';"
  , "  " <> q systemPath <> "/bin/switch-to-configuration boot;"
  , "  nohup bash -c 'sleep 2 && reboot' &>/dev/null &;"
  , "else"
  , "  echo '==> Switching configuration...';"
  , "  " <> q systemPath <> "/bin/switch-to-configuration switch;"
  , "fi"
  ]
  where q s = "'" <> s <> "'"
