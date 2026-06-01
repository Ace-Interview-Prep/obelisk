{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Common.View where

#if !defined(javascript_HOST_ARCH) && !defined(wasm32_HOST_ARCH)
import Common.Schema
import Common.Request
import Data.Signed
import Data.Functor.Identity (Identity)
import Database.Beam.Schema (PrimaryKey)

import Rhyolite.Account
import Data.Vessel.Void
import Rhyolite.Vessel.App

import Common.Route (Id)

type ChatAuthCredential = Signed (PrimaryKey Account Identity)

data JengaApp

instance RhyoliteAuthApp JengaApp where
  type AuthCredential JengaApp = Signed (Id Account)

  type PublicApi JengaApp = PublicRequest
  type PrivateApi JengaApp = PrivateRequest

  -- Use VoidV for all vessel types until the app has real queries.
  -- This satisfies all Group/View/EmptyView constraints.
  type PrivateV JengaApp = VoidV
  type PersonalV JengaApp = VoidV
  type PublicV JengaApp = VoidV

#endif
