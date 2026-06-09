-- | Development entry point for ghcid. NOT listed in backend.cabal —
-- only used interpreted via ghcid's :add.
module DevMain (devMain) where

import Jenga.Backend.Servant

import Backend (backend)
import Paths (dataFileName)

devMain :: IO ()
devMain = runBackendWith config backend
  where
    config = BackendConfig
      { _backendConfig_runSnap = runSnapWithCommandLineArgs
      , _backendConfig_staticAssets = StaticAssets
          { _staticAssets_processed = dataFileName "static.assets"
          , _staticAssets_unprocessed = dataFileName "static"
          }
      , _backendConfig_frontendAssets = StaticAssets
          { _staticAssets_processed = dataFileName "frontend.jsexe.assets"
          , _staticAssets_unprocessed = dataFileName "frontend.jsexe"
          }
      , _backendConfig_ghcjsWidgets = defaultGhcjsWidgets
      }
