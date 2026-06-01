module Reflex.Effectful.Effect.Jenga.Configs
  ( Configs(..)
  , getConfigs
  , getConfig
  , getTextConfig
  ) where

import           Data.ByteString          (ByteString)
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Text                (Text)
import qualified Data.Text.Encoding       as T

import           Effectful                (Effect, Dispatch(..), DispatchOf, (:>), Eff)
import           Effectful.Dispatch.Dynamic (send)

data Configs :: Effect where
  GetConfigs :: Configs m (Map Text ByteString)

type instance DispatchOf Configs = 'Dynamic

getConfigs :: Configs :> es => Eff es (Map Text ByteString)
getConfigs = send GetConfigs

getConfig :: Configs :> es => Text -> Eff es (Maybe ByteString)
getConfig k = Map.lookup k <$> getConfigs

getTextConfig :: Configs :> es => Text -> Eff es (Maybe Text)
getTextConfig k = fmap T.decodeUtf8 <$> getConfig k
