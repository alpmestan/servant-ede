{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.EDE
import Text.EDE

data User = User String Int
  deriving (Eq, Show)

instance ToObject User where
	toObject (User name age) = fromPairs [ "name" .= name
                                       , "age"  .= age
                                       ]

type DummyAPI = "index" :> Tpl "index.tpl"
           :<|> "foo" :> Tpl "foo.tpl"

dummyAPI :: Proxy DummyAPI
dummyAPI = Proxy

type UserAPI = "user" :> Get '[HTML "user.tpl"] User

userAPI :: Proxy UserAPI
userAPI = Proxy

type API = UserAPI :<|> DummyAPI

api :: Proxy API
api = Proxy

server :: Templates -> Server API
server ts = return (User "lambdabot" 35) :<|> ts :<|> ts

main :: IO ()
main = do
  res <- loadTemplates dummyAPI "example"
  case res of
    Left errs -> forM_ errs print
    Right tmap -> do
      loadTemplates_ userAPI "example"
      run 8082 (serve api $ server tmap)
