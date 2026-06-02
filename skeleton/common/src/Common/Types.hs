{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Common.Types where

#if !defined(javascript_HOST_ARCH) && !defined(wasm32_HOST_ARCH)
import Jenga.Common.Errors
#endif
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..), withText)
import Text.Email.Validate (EmailAddress)

instance ToJSON BS.ByteString where
  toJSON = toJSON . TE.decodeUtf8

instance FromJSON BS.ByteString where
  parseJSON = withText "ByteString" (pure . TE.encodeUtf8)

data NameError = NotFound | NameError T.Text
  deriving stock (Show, Eq, Generic)

instance ToJSON NameError
instance FromJSON NameError

instance ToJSON EmailAddress
instance FromJSON EmailAddress

instance SpecificError (BackendError NameError)
