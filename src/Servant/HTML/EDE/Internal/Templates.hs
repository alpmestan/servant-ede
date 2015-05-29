{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Servant.HTML.EDE.Internal.Templates
  ( Tpl
  , Templates
  , TemplateFiles
  , TemplateError
  , Errors
  , processFile
  , templateFiles
  ) where

import Control.Monad.IO.Class
import Data.ByteString.Lazy.Char8 (pack)
import Data.HashMap.Strict
import Data.Proxy
import Data.Semigroup
import Data.Text.Lazy.Encoding (encodeUtf8)
import GHC.TypeLits
import Network.HTTP.Types
import Network.Wai
import Servant.HTML.EDE.Internal.Reify
import Servant.HTML.EDE.Internal.Validate
import Servant
import Servant.Server.Internal
import System.FilePath
import Text.EDE

import qualified Data.HashMap.Strict as HM

-- | Combinator for serving EDE templates without arguments. Usage:
--
-- > type API = "index" :> Tpl "index.tpl"
-- >       :<|> "about" :> Tpl "about.tpl"
-- >
-- > api :: Proxy API
-- > api = Proxy
-- >
-- > server :: Templates -> Server API
-- > server tpls = tpls :<|> tpls
-- >
-- > main :: IO ()
-- > main = do
-- >   tpls <- loadTemplates api "./templates"
-- >   run 8080 (serve api $ server tpls)
data Tpl (tplfile :: Symbol)

-- | The so-called "request handler" for an endpoint ending
--   with 'Tpl' just has to be the opaque 'Templates' value
--   returned by 'Servant.HTML.EDE.loadTemplates' applied to your API, which
--   is just a compiled template store indexed by file name.
instance KnownSymbol tplfile => HasServer (Tpl tplfile) where
  type ServerT (Tpl tplfile) m = Templates

  route Proxy (Templates templateMap) request respond
    | pathIsEmpty request && requestMethod request == methodGet =
        case mbody of
          Success body -> respond . succeedWith $
            responseLBS ok200 [("Content-Type", "text/html")] (encodeUtf8 body)
          Failure doc  -> respond . succeedWith $
            responseLBS status500 [] ("template error: " <> pack (show doc))
    | pathIsEmpty request && requestMethod request /= methodGet =
        respond (failWith WrongMethod)
    | otherwise = respond (failWith NotFound)

    where filename = symbolVal (Proxy :: Proxy tplfile)
          mbody = render (templateMap HM.! filename) mempty

type family Append (xs :: [k]) (ys :: [k]) :: [k] where
  Append '[]       ys = ys
  Append (x ': xs) ys = x ': Append xs ys

type family Member (x :: k) (xs :: [k]) :: Bool where
  Member x (x ': xs) = 'True
  Member x (y ': xs) = Member x xs
  Member x       '[] = False

-- | Collect all the template filenames of an API as a type-level
--   list of strings, by simply looking at all occurences of the
--   'Tpl' combinator and keeping the filename associated to it.
type family TemplateFiles (api :: k) :: [Symbol] where
  TemplateFiles (a :<|> b) = Append (TemplateFiles a) (TemplateFiles b)
  TemplateFiles (a :> r)   = TemplateFiles r
  TemplateFiles (Tpl f)    = '[f]
  TemplateFiles a          = '[]

templates :: Proxy api -> Proxy (TemplateFiles api)
templates Proxy = Proxy

templateFiles :: Reify (TemplateFiles api) => Proxy api -> [FilePath]
templateFiles = reify . templates

-- | An opaque "compiled-template store".
--
-- The only way to get a value of this type is to use
-- 'Servant.EDE.loadTemplates' on a proxy of your API.
--
-- This ensures that when we lookup a template (in order
-- to render it) in our 'Templates' store, we are
-- guaranteed to find it.
newtype Templates = Templates (HashMap String Template)
  deriving Eq

instance Semigroup Templates where
  Templates a <> Templates b = Templates (a <> b)

instance Monoid Templates where
  mempty = Templates mempty

  a `mappend` b = a <> b

tpl :: FilePath -> Template -> Templates
tpl fp t = Templates $ HM.singleton fp t

-- | A 'TemplateError' is a pair of a template filename
--   and the error string for that file.
type TemplateError = (FilePath, String)

-- | A list of 'TemplateError's.
type Errors = [TemplateError]

err :: Show a => FilePath -> a -> Errors
err fp d = [(fp, show d)]

processFile :: MonadIO m => FilePath -> FilePath -> ValidateT Errors m Templates
processFile d fp = validate . liftIO $ parseFile' (d </> fp)

  where parseFile' f = fmap validateResult (parseFile f)
        validateResult (Success t) = OK (tpl fp t)
        validateResult (Failure e) = NotOK (err fp e)
