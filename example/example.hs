{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
import Control.Monad
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.EDE

type API = "index" :> Tpl "index.tpl"
      :<|> "foo" :> Tpl "foo.tpl"

api :: Proxy API
api = Proxy

server :: Templates -> Server API
server ts = ts :<|> ts 

main :: IO ()
main = do
  tpls <- loadTemplates api "example"
  case tpls of
    Left es  -> forM_ es print
    Right ts -> run 8082 (serve api $ server ts)
