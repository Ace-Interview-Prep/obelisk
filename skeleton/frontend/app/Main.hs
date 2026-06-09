{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeApplications #-}

import Data.Proxy (Proxy(..))
import qualified Data.Map as Map
import Jenga.Frontend (runFrontend)
import Common.Route
import Frontend

#if defined(wasm32_HOST_ARCH)
import Language.Javascript.JSaddle.Wasm (run)
#elif !defined(ghcjs_HOST_OS)
import qualified Language.Javascript.JSaddle.Warp as JW
#endif

#if defined(ghcjs_HOST_OS)
main :: IO ()
main = runFrontend (Proxy @FrontendPages) FrontendRoute_Main Map.empty frontend
#elif defined(wasm32_HOST_ARCH)
main :: IO ()
main = run $ runFrontend (Proxy @FrontendPages) FrontendRoute_Main Map.empty frontend
foreign export javascript "hs_start" main :: IO ()
#else
main :: IO ()
main = JW.run 3003 $ runFrontend (Proxy @FrontendPages) FrontendRoute_Main Map.empty frontend
#endif
