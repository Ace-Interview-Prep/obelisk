module Jenga.Backend.Utils.HasTable where

import Jenga.Backend.Utils.Query
import Jenga.Backend.Utils.HasConfig
import Database.Beam.Postgres
import Database.Beam.Schema
import Data.Pool

import Effectful (Eff, (:>), IOE)
import Effectful.Reader.Static (Reader)


class HasJengaTable dbHost db tbl where
  tableRef :: PgTable dbHost db tbl

type PgTable dbHost db x = DatabaseEntity dbHost db (TableEntity x)

asksTableM
  :: (HasJengaTable dbHost db tbl)
  => Eff es (PgTable dbHost db tbl)
asksTableM = do
  pure tableRef

withDbEnv :: (Reader cfg :> es, HasConfig cfg (Pool Connection), IOE :> es) => Pg a -> Eff es a
withDbEnv query_ = flip runSerializable query_ =<< asksM

withDbEnvQuiet  :: (Reader cfg :> es, HasConfig cfg (Pool Connection), IOE :> es) => Pg a -> Eff es a
withDbEnvQuiet query = do
  dbConn <- asksM
  runDbQuiet dbConn query
