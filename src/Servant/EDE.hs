{-# LANGUAGE DataKinds #-}
module Servant.EDE where

import Control.Concurrent
import Data.HashMap.Strict (HashMap, (!))
import Data.Text.Encoding (encodeUtf8)
import GHC.TypeLits
import Servant.API.ContentTypes
import System.IO.Unsafe
import Text.EDE

data Tpl (ct :: *) (file :: Symbol)

-- the filename doesn't matter for the content type,
-- as long as 'ct' is a valid one (html, json, css, application-specific, whatever really)
instance Accept ct => Accept (Tpl ct file) where
  accept _ = accept ctproxy
    where ctproxy = Proxy :: Proxy ct

instance (KnownSymbol file, Accept ct, ToObject a) => MimeRender (Tpl ct file) a where
  mimeRender _ val = encodeUtf8 . result (error . show) id $
    render templ (toObject val)

    where templ = tmap ! filename
          filename = symbolVal (Proxy :: Proxy tplfile)
          tmap = templateMap $ unsafePerformIO (readMVar __template_store)

__template_store :: MVar Templates
__template_store = unsafePerformIO newEmptyMVar
