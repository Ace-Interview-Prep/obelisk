{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Common.Request where

#if !defined(javascript_HOST_ARCH) && !defined(wasm32_HOST_ARCH)
import Jenga.Common.Errors
import Jenga.Common.Auth

import Data.Aeson ()
import Data.Aeson.GADT.TH
import Data.Constraint.Extras.TH

import Common.Types
import Text.Email.Validate

-- | Requests requiring the user to be logged in.
data PrivateRequest a where
  PrivateRequest_Names
    :: PrivateRequest (Either (BackendError NameError) Email)

type AuthPayload = (AuthToken, UserType)

-- | Requests that do not require the user to be logged in.
data PublicRequest a where
  PublicRequest_RequestResetPassword
    :: Email
    -> PublicRequest (Either (BackendError RequestPasswordResetError) ())
  PublicRequest_Signup
    :: EmailAddress
    -> PublicRequest (Either (BackendError UserSignupError) ())

deriveArgDict ''PrivateRequest
deriveJSONGADT ''PrivateRequest
deriveArgDict ''PublicRequest
deriveJSONGADT ''PublicRequest
#endif
