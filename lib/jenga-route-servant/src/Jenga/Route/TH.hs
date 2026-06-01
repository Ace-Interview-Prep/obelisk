{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Template Haskell for deriving 'HasRoute' instances.
--
-- Generates 'encodeRoute' and 'decodeRoute' from a simple
-- route specification.
--
-- @
-- -- Define routes as a list of (constructor, path segments, captures)
-- mkRoutes "FrontendRoute" ''FrontendPages
--   [ ("FR_Main",          [])
--   , ("FR_Login",         ["login"])
--   , ("FR_Signup",        ["signup"])
--   , ("FR_ResetPassword", ["reset-password"])
--   , ("FR_Dashboard",     ["dashboard", capture @Int])
--   , ("FR_Settings",      ["settings"])
--   ]
-- @
--
-- This generates:
-- 1. The @FrontendRoute@ data type
-- 2. The @HasRoute FrontendPages FrontendRoute@ instance
--
-- For now this is a placeholder — the TH implementation is complex
-- and best done after the core API stabilizes. Users write instances
-- by hand (they're short — see CommonRoute.hs example).
module Jenga.Route.TH
  ( -- * Placeholder
    -- mkRoutes
  ) where

-- TH derivation will be implemented once the core API stabilizes.
-- For now, HasRoute instances are hand-written. They're short:
--
-- instance HasRoute FrontendPages FrontendRoute where
--   encodeRoute = \case
--     FR_Main -> "/"
--     FR_Login -> "/login"
--   decodeRoute segs _ = case segs of
--     [] -> Just FR_Main
--     ["login"] -> Just FR_Login
--     _ -> Nothing
