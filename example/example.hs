{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Data.Monoid
import GHC.Generics
import Network.HTTP.Media ((//))
import Network.Wai.Handler.Warp
import Servant
import Servant.EDE
import Text.EDE.Filters ((@:),Term)
import qualified Data.HashMap.Strict as Map
import Data.Text (Text, chunksOf)

-- * Using 'Tpl' for rendering CSS templates

data CSS

instance Accept CSS where
  contentType _ = "text" // "css"

type StyleAPI = "style.css" :> Get '[Tpl CSS "style.tpl"] CSSData

data CSSData = CSSData
  { darken :: Bool
  , pageWidth :: Int
  } deriving Generic

instance ToObject CSSData

styleServer :: Server StyleAPI
styleServer = -- FOR NOW
  return (CSSData True 400)

-- * Using 'HTML' for HTML template rendering

data User = User { name :: String, age :: Int }
  deriving (Eq, Show, Generic)

instance ToObject User where

type UserAPI = "user" :> Get '[HTML "user.tpl"] User

userServer :: Server UserAPI
userServer = return (User "lambdabot" 35)

type API = StyleAPI :<|> UserAPI

api :: Proxy API
api = Proxy

filters :: [(Text,Term)]
filters = ["toChars" @: (chunksOf 1)]

main :: IO ()
main = do
  loadTemplates api filters "example"
  run 8082 (serve api $ styleServer :<|> userServer)

-- You can now head to:
-- http://localhost:8082/user
-- and
-- http://localhost:8082/style.css
-- to see 'HTML' and 'Tpl' + 'CSS' in action,
-- respectively.
--
-- Feel free to tweak the content of the template files
-- as well as the 'User' and 'CSSData' values in this program
-- to see how it affects the rendering.
