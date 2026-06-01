module Reflex.Effectful.Effect.Jenga.Cookies
  ( CookiesEff(..)
  , askCookies
  ) where

import           Data.ByteString          (ByteString)
import           Effectful                (Effect, Dispatch(..), DispatchOf, (:>), Eff)
import           Effectful.Dispatch.Dynamic (send)

-- | Cookie type (list of name-value pairs).
-- Re-defined here to avoid depending on cookie package directly.
type Cookies = [(ByteString, ByteString)]

data CookiesEff :: Effect where
  AskCookies :: CookiesEff m Cookies

type instance DispatchOf CookiesEff = 'Dynamic

askCookies :: CookiesEff :> es => Eff es Cookies
askCookies = send AskCookies
