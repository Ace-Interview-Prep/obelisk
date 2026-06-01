{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module Common.Schema (module Common.Schema, module X) where

import Common.SchemaHelpers as X

#if !defined(javascript_HOST_ARCH) && !defined(wasm32_HOST_ARCH)
import Jenga.Common.Schema
import Rhyolite.Account
import Database.Beam

data Db f = Db
  { _db_accounts :: f (TableEntity Account)
  , _db_sendEmailTask :: f (TableEntity SendEmailTask)
  , _db_reporting :: f (TableEntity LogItemRow)
  , _db_validOrgEmails :: f (TableEntity OrganizationEmails)
  , _db_userType :: f (TableEntity UserTypeTable)
  } deriving stock (Generic)

instance Database be Db
#endif
