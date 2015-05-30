{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Data.Monoid
import GHC.Generics
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.EDE
import Text.EDE

data User = User { name :: String, age :: Int }
  deriving (Eq, Show, Generic)

instance ToObject User where

type API = "index" :> Tpl "index.tpl"
      :<|> "foo"   :> Tpl "foo.tpl"
      :<|> "user"  :> Get '[HTML "user.tpl"] User

api :: Proxy API
api = Proxy

server :: Server API
server = rawTemplate :<|> rawTemplate :<|> return (User "lambdabot" 35)

  where rawTemplate = return mempty

main :: IO ()
main = do
  loadTemplates api "example"
  run 8082 (serve api server)
