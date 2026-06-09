import qualified Data.Text as T
import Jenga.Asset.Cabal
import Jenga.Asset.Gather
import Jenga.Asset.Symlink
import System.Environment
import System.FilePath

main :: IO ()
main = do
  --TODO: Usage
  [root, haskellTarget, packageName, moduleName, fileTarget] <- getArgs
  paths <- gatherHashedPaths root
  writeCabalProject haskellTarget $ SimplePkg
    { _simplePkg_name = T.pack packageName
    , _simplePkg_moduleName = T.pack moduleName
    , _simplePkg_dependencies = map T.pack
      [ "base"
      , "jenga-asset-manifest"
      , "template-haskell"
      ]
    , _simplePkg_moduleContents = T.pack $ unlines
      [ "{-# Language CPP #-}"
      , "{-|"
      , "  Description:"
      , "    Automatically generated module that provides the 'static' TH function"
      , "    to generate paths to static assets."
      , "-}"
      , "module " <> moduleName <> " ( static, staticFilePath ) where"
      , ""
      , "import Jenga.Asset.TH"
      , "import Language.Haskell.TH"
      , ""
      , "static, staticFilePath :: FilePath -> Q Exp"
      , "#ifdef JENGA_ASSET_PASSTHRU"
      , "static = staticAssetRaw"
      , "staticFilePath =  staticAssetFilePathRaw \"static.out\""
      , "#else"
      , "static = staticAssetHashed " <> show root
      , "staticFilePath = staticAssetFilePath " <> show root
      , "#endif"
      ]
    }
  copyAndSymlink paths root fileTarget
