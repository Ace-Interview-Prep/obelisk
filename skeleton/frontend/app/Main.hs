{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeApplications #-}

import Data.Proxy (Proxy(..))
import Jenga.Frontend
import Common.Route
import Frontend

main :: IO ()
main = runFrontend (Proxy @FrontendPages) FrontendRoute_Main mempty frontend

#if defined(wasm32_HOST_ARCH)
foreign export javascript "hs_start" main :: IO ()
#endif
