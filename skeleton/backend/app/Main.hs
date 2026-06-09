import Jenga.Backend.Servant
import System.FilePath ((</>))

import Backend
import Paths (dataDir)

main :: IO ()
main = runBackendWith config backend
  where
    config = BackendConfig
      { _backendConfig_runSnap = runSnapWithCommandLineArgs
      , _backendConfig_staticAssets = StaticAssets
          { _staticAssets_processed = dataDir </> "static.assets"
          , _staticAssets_unprocessed = dataDir </> "static"
          }
      , _backendConfig_frontendAssets = StaticAssets
          { _staticAssets_processed = dataDir </> "frontend.jsexe.assets"
          , _staticAssets_unprocessed = dataDir </> "frontend.jsexe"
          }
      , _backendConfig_ghcjsWidgets = defaultGhcjsWidgets
      }
