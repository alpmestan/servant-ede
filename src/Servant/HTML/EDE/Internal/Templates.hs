{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Servant.HTML.EDE.Internal.Templates
  ( Tpl
  , HTML
  , Templates(..)
  , templateMap
  , __template_store
  , TemplateFiles
  , TemplateError
  , Errors
  , processFile
  , templateFiles
  ) where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Aeson (Object)
import Data.ByteString.Lazy.Char8 (pack)
import Data.HashMap.Strict
import Data.Proxy
import Data.Semigroup
import Data.Text.Lazy.Encoding (encodeUtf8)
import GHC.TypeLits
import Network.HTTP.Media hiding (Accept)
import Network.HTTP.Types
import Network.Wai
import Servant
import Servant.HTML.EDE.Internal.Reify
import Servant.HTML.EDE.Internal.ToObject
import Servant.HTML.EDE.Internal.Validate
import Servant.Server.Internal
import Servant.Server.Internal.ServantErr
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)
import Text.EDE

import qualified Data.HashMap.Strict as HM

__template_store :: MVar Templates
__template_store = unsafePerformIO newEmptyMVar

-- | Combinator for serving EDE templates without arguments. Usage:
--
-- > type API = "index" :> Tpl "index.tpl"
-- >       :<|> "about" :> Tpl "about.tpl"
-- >
-- > api :: Proxy API
-- > api = Proxy
-- >
-- > server :: Templates -> Server API
-- > server tpls = return mempty :<|> return mempty
-- >
-- > main :: IO ()
-- > main = do
-- >   loadTemplates_ api "./templates"
-- >   run 8080 (serve api server)
data Tpl (tplfile :: Symbol)


-- | The so-called "request handler" for an endpoint ending
--   with 'Tpl' just has to be the opaque 'Templates' value
--   returned by 'Servant.HTML.EDE.loadTemplates' applied to your API, which
--   is just a compiled template store indexed by file name.
instance KnownSymbol tplfile => HasServer (Tpl tplfile) where
  type ServerT (Tpl tplfile) m = m Object

  route Proxy mobj request respond
    | pathIsEmpty request && requestMethod request == methodGet = do
        tpls <- getTemplates
        val <- runEitherT mobj
        case val of
          Left e -> respond . succeedWith $ responseServantErr e
          Right v  -> 
            case mbody tpls v of
              Success body -> respond . succeedWith $
                responseLBS ok200 [("Content-Type", "text/html")] (encodeUtf8 body)
              Failure doc  -> respond . succeedWith $
                responseLBS status500 [] ("template error: " <> pack (show doc))

    | pathIsEmpty request && requestMethod request /= methodGet =
        respond (failWith WrongMethod)

    | otherwise = respond (failWith NotFound)

    where filename = symbolVal (Proxy :: Proxy tplfile)
          mbody ts val = render (ts HM.! filename) val
          getTemplates = fmap templateMap (readMVar __template_store)

-- | 'HTML' content type, but more than just that.
--
--   'HTML' takes a type-level string which is
--   a filename for the template you want to use to
--   render values. Example:
--
-- @
-- type UserAPI = "user" :> Get '[JSON, HTML "user.tpl"] User
--
-- userAPI :: Proxy UserAPI
-- userAPI = Proxy
--
-- data User = User { name :: String, age :: Int } deriving Generic
--
-- instance ToJSON User
-- instance ToObject User
--
-- server :: Server API
-- server = return (User "lambdabot" 31)
--
-- main :: IO ()
-- main = do
--   loadTemplates userAPI "./templates"
--   run 8082 (serve userAPI server)
-- @
--
-- This will look for a template at @.\/templates\/user.tpl@, which could
-- for example be:
--
-- > <ul>
-- >   <li><strong>Name:</strong> {{ name }}</li>
-- >   <li><strong>Age:</strong> {{ age }}</li>
-- > </ul>
data HTML (tplfile :: Symbol)

-- | @text\/html;charset=utf-8@
instance Accept (HTML tplfile) where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance (KnownSymbol tplfile, ToObject a)
      => MimeRender (HTML tplfile) a where
  mimeRender _ val = encodeUtf8 . result (error . show) id $
    render templ (toObject val)

    where templ = tmap ! filename
          filename = symbolVal (Proxy :: Proxy tplfile)
          tmap = templateMap $ unsafePerformIO (readMVar __template_store)

type family Append (xs :: [k]) (ys :: [k]) :: [k] where
  Append '[]       ys = ys
  Append (x ': xs) ys = x ': Append xs ys

type family Member (x :: k) (xs :: [k]) :: Bool where
  Member x (x ': xs) = 'True
  Member x (y ': xs) = Member x xs
  Member x       '[] = False

-- | Collect all the template filenames of an API as a type-level
--   list of strings, by simply looking at all occurences of the
--   'Tpl' and 'HTML' combinators and keeping the filenames associated to them.
type family TemplateFiles (api :: k) :: [Symbol]
type instance TemplateFiles (a :<|> b)    = Append (TemplateFiles a) (TemplateFiles b)
type instance TemplateFiles (a :> r)      = TemplateFiles r
type instance TemplateFiles (Tpl f)       = '[f]
type instance TemplateFiles (Delete cs a) = CTFiles cs
type instance TemplateFiles (Get cs a)    = CTFiles cs
type instance TemplateFiles (Patch cs a)  = CTFiles cs
type instance TemplateFiles (Post cs a)   = CTFiles cs
type instance TemplateFiles (Put cs a)    = CTFiles cs

type family CTFiles (cts :: [*]) :: [Symbol] where
  CTFiles '[]        = '[]
  CTFiles (c ': cts) = Append (CTFile c) (CTFiles cts)

type family CTFile c :: [Symbol] where
  CTFile (HTML fp) = '[fp]
  CTFile         a = '[]

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

templateMap :: Templates -> HashMap String Template
templateMap (Templates m) = m

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
