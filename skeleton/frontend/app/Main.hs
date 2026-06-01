{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeApplications #-}

import Data.Proxy (Proxy(..))
import qualified Data.Map as Map
import Jenga.Frontend (runFrontend)
import Common.Route
import Frontend

#if defined(ghcjs_HOST_OS) || defined(wasm32_HOST_ARCH)
main :: IO ()
main = runFrontend (Proxy @FrontendPages) FrontendRoute_Main Map.empty frontend
#else
import qualified Language.Javascript.JSaddle.Warp as JW
main :: IO ()
main = JW.run 3003 $ runFrontend (Proxy @FrontendPages) FrontendRoute_Main Map.empty frontend
#endif

#if defined(wasm32_HOST_ARCH)
foreign export javascript "hs_start" main :: IO ()
#endif
