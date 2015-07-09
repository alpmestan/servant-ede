{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Data.Monoid
import GHC.Generics
import Network.Wai.Handler.Warp
import Servant
import Servant.EDE

data User = User { name :: String, age :: Int }
  deriving (Eq, Show, Generic)

instance ToObject User where

type API = "user" :> Get '[HTML "user.tpl"] User

api :: Proxy API
api = Proxy

server :: Server API
server = return (User "lambdabot" 35)

main :: IO ()
main = do
  loadTemplates api "example"
  run 8082 (serve api server)
