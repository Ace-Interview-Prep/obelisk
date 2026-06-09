{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Jenga.Frontend.Auth.UserSignup where

import Jenga.Common.Auth
import Templates.Partials.Checkbox
import Templates.Types
import Jenga.Common.Errors
import Jenga.Route.Frontend (SetRoute, RouteToUrl)
import Reflex (Reflex, Event, Dynamic, current, tag, leftmost, ffor, fanEither, (<$))

import Control.Monad (forM_)
import qualified Text.Email.Validate as EmailValidate
import qualified Data.Text.Encoding as T
import qualified Data.Text as T

import Effectful (Eff, (:>))
import Reflex.Effectful.Effect.Hold (Hold, holdDyn)
import Reflex.Effectful.Effect.Dom (Dom, el, elClass, text)
import Reflex.Effectful.Effect.PostBuild (PostBuild)
import Reflex.Effectful.Effect.Prerender (Prerender)

data UserSignupConfig t = UserSignupConfig
  { _signupConfig_errors :: Dynamic t (Maybe T.Text)
  , _signupConfig_success :: Event t ()
  }

data UserSignupData t m = UserSignupData
  { _signup_email :: InputEl t m
  , _signup_confirmEmail :: InputEl t m
  , _signup_agreeToTerms :: Checkbox t
  , _signup_submit :: Event t ()
  }

newUserSignup_FRP
  :: forall req rsp t es.
     ( Dom t :> es
     , Hold t :> es
     , PostBuild t :> es
     , Prerender t :> es
     , Reflex t
     )
  => (EmailValidate.EmailAddress -> req)
  -> (Event t req -> Eff es (Event t rsp))
  -> UserSignupData t (Eff es)
  -> Eff es (UserSignupConfig t)
newUserSignup_FRP mkAPI sendRequest (UserSignupData email' eConfirm' agree' submit) = do
  let email = value email'
  let eConfirm = value eConfirm'
  let agree = value agree'
  let
    validate (emailA, emailB, agree'') =
      if not agree''
      then Left "You must accept terms"
      else if emailA /= emailB
      then Left "Emails dont match"
      else case EmailValidate.validate $ T.encodeUtf8 emailA of
        Left _ -> Left "Invalid Email"
        Right e -> Right e
    valid = fmap validate $ (,,) <$> email <*> eConfirm <*> agree
  let (bad, good') = fanEither (tag (current valid) submit)
  let request = ffor good' mkAPI
  response <- sendRequest request
  let (errRes, goodResponse) = fanEither (castResponse response)
  let errorsEv = Just <$> leftmost
        [ bad
        , showUser . Req . Request_ErrorAPI <$> errRes
        , "Success, please check your email" <$ goodResponse
        ]
  errors <- holdDyn Nothing errorsEv
  pure $ UserSignupConfig errors (() <$ goodResponse)
  where
    value = undefined -- TODO: InputEl accessor
    castResponse = undefined -- TODO: cast response type

seeTerms :: (Dom t :> es, Reflex t) => Eff es ()
seeTerms = do
  elClass "div" "bg-gray-100 flex items-center justify-center" $ do
    elClass "div" "bg-white p-8 rounded-3xl w-full overflow-y-auto h-[50vh]" $ do
      elClass "h1" "text-2xl font-[Sarabun] mb-4 text-gray-700" $ text "TERMS OF SERVICE AND PRIVACY POLICY"
      elClass "p" "text-base font-[Sarabun] mb-2 text-gray-700" $ text "Last Updated: March 23, 2023"
      forM_ paragraphsAgree $ elClass "p" "text-base font-[Sarabun] mb-2 text-gray-700" . text
  where
    paragraphsAgree =
      [ "This document (\"Agreement\") is an agreement between you (\"User\" or \"You\") and Ace, Inc., (\"Company,\" \"We,\" \"Us,\" or \"Our\"), governing your use of the Ace, Inc. platform and related services (collectively, the \"Services\")."
      , "By accessing or using the Services, you acknowledge that you have read, understood, and agree to be bound by this Agreement. If you do not agree to these terms, you are not permitted to use the Services."
      ]
