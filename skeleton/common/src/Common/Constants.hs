{-# LANGUAGE OverloadedStrings #-}
module Common.Constants where

import Data.Text (Text)

authCookieName :: Text
authCookieName = "cookie_ip"

userTypeCookieName :: Text
userTypeCookieName = "utype"
