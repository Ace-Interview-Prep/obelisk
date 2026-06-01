-- | Development entry point for ghcid. NOT listed in backend.cabal —
-- only used interpreted via multi-repl where Frontend is in scope.
module DevMain (devMain) where

import Jenga.Backend
import System.FilePath ((</>))

import Backend (backend)
import Frontend (frontend)
import Paths (dataDir)

devMain :: IO ()
devMain = runBackendWith config backend frontend
  where
    config = BackendConfig
      { _backendConfig_runSnap = runSnapWithCommandLineArgs
      , _backendConfig_staticAssets = StaticAssets
          { _staticAssets_processed = dataDir </> "static.assets"
          , _staticAssets_unprocessed = dataDir </> "static"
          }
      , _backendConfig_frontendGhcjsAssets = StaticAssets
          { _staticAssets_processed = dataDir </> "frontend.jsexe.assets"
          , _staticAssets_unprocessed = dataDir </> "frontend.jsexe"
          }
      , _backendConfig_ghcjsWidgets = defaultGhcjsWidgets
      }
