{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Servant.EDE
-- Copyright   :  (c) Alp Mestanogullari 2015
-- Maintainer  :  alpmestan@gmail.com
-- Stability   :  experimental
--
-- Rendering EDE templates with servant.
--
-- This package provides two combinators to be used as content-types
-- with servant (i.e just like 'JSON'), 'HTML' and 'Tpl'.
--
-- - 'HTML' takes a filename as parameter and lets you render the template
--   with that name against the data returned by a request handler using
--   the @text\/html;charset=utf-8@ MIME type, XSS-sanitizing the said data
--   along the way. See 'HTML' for an example.
-- - 'Tpl' does the same except that it's parametrized over the content type
--   to be sent along with the rendered template. Any type that has an 'Accept'
--   instance will do. See 'Tpl' for an example.
-----------------------------------------------------------------------------
module Servant.EDE
  ( -- * Combinators
    HTML
  , Tpl

    -- * Sending Haskell data to templates
  , ToObject(..)

  , -- * Loading template files (mandatory)
    loadTemplates
  , TemplateFiles
  , Reify
  , Templates
  , Errors
  , TemplateError
  ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
import Data.Traversable (traverse)
#endif

import Control.Concurrent
import Control.Monad.IO.Class
import Data.Aeson (Object, Value(..))
import Data.Foldable (fold)
import Data.HashMap.Strict (HashMap, (!))
import Data.Proxy
import Data.Semigroup
import Data.Text (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)
import GHC.TypeLits
import Network.HTTP.Media hiding (Accept)
import Servant.API
import Servant.EDE.Internal.Reify
import Servant.EDE.Internal.ToObject
import Servant.EDE.Internal.Validate
import System.FilePath
import System.IO.Unsafe
import Text.EDE
import Text.HTML.SanitizeXSS

import qualified Data.HashMap.Strict as HM
import qualified Data.Vector         as V

-- | This function initializes a global
--   template store (i.e a 'Templates' value) and fills it with
--   the resulting compiled templates if all of them are compiled
--   successfully. If that's not the case, the global template store
--   (under an 'MVar') is left empty.
--
--   /IMPORTANT/: Must /always/ be called before starting your /servant/ application. Example:
--
-- > type API = Get '[HTML "home.tpl"] HomeData
-- >
-- > api :: Proxy API
-- > api = Proxy
-- >
-- > main :: IO ()
-- > main = loadTemplates api "path/to/templates" >>= print
--
-- This would try to load @home.tpl@, printing any error or
-- registering the compiled template in a global (but safe)
-- compiled template store, if successfully compiled.
loadTemplates :: (Reify (TemplateFiles api), Applicative m, MonadIO m)
              => Proxy api
              -> FilePath -- ^ root directory for the templates
              -> m Errors
loadTemplates proxy dir = do
  res <- loadTemplates' proxy dir
  case res of
    Left errs  -> return errs
    Right tpls -> do
      liftIO $ putMVar __template_store tpls
      return []

loadTemplates' :: (Reify (TemplateFiles api), Applicative m, MonadIO m)
               => Proxy api
               -> FilePath   -- ^ root directory for the templates
               -> m (Either Errors Templates)
loadTemplates' proxy templatedir =
  fmap (eitherValidate . fmap fold) . runValidateT $
    traverse (processFile templatedir) files

  where files :: [FilePath]
        files = templateFiles proxy

-- | A generic template combinator, parametrized over
--   the content-type (or MIME) associated to the template.
--
--   The first parameter is the content-type you want to send along with
--   rendered templates (must be an instance of 'Accept').
--
--   The second parameter is the name of (or path to) the template file.
--   It must live under the 'FilePath' argument of 'loadTemplates'.
--
--   Any type used with this content-type (like @CSSData@ below)
--   must have an instance of the 'ToObject' class. The field names
--   become the variable names in the template world.
--
--   Here is how you could render and serve, say, /CSS/
--   (Cascading Style Sheets) templates that make use
--   of some @CSSData@ data type to tweak the styling.
--
-- @
-- data CSS
--
-- instance Accept CSS where
--   contentType _ = "text" // "css"
--
-- type StyleAPI = "style.css" :> Get '[Tpl CSS "style.tpl"] CSSData
--
-- styleAPI :: Proxy StyleAPI
-- styleAPI = Proxy
--
-- data CSSData = CSSData
--   { darken :: Bool
--   , pageWidth :: Int
--   } deriving Generic
--
-- instance ToObject CSSData
--
-- server :: Server API
-- server = -- produce a CSSData value depending on whatever is relevant...
--
-- main :: IO ()
-- main = do
--   loadTemplates styleAPI "./templates"
--   run 8082 (serve styleAPI server)
-- @
--
-- This will look for a template at @.\/templates\/style.tpl@,
-- which could for example be:
--
-- > body {
-- >   {% if darken %}
-- >   background-color: #222222;
-- >   color: blue;
-- >   {% else %}
-- >   background-color: white;
-- >   color: back;
-- >   {% endif %}
-- > }
-- >
-- > #content {
-- >   width: {{ pageWidth }};
-- >   margin: 0 auto;
-- > }
--
-- A complete, runnable version of this can be found
-- in the @examples@ folder of the git repository.
data Tpl (ct :: *) (file :: Symbol)

-- the filename doesn't matter for the content type,
-- as long as 'ct' is a valid one (html, json, css, etc or application-specific)
instance Accept ct => Accept (Tpl ct file) where
  contentType _ = contentType ctproxy
    where ctproxy = Proxy :: Proxy ct

instance (KnownSymbol file, Accept ct, ToObject a) => MimeRender (Tpl ct file) a where
  mimeRender _ val = encodeUtf8 . result (error . show) id $
    render templ (toObject val)

    where templ = tmap ! filename
          filename = symbolVal (Proxy :: Proxy file)
          tmap = templateMap $ unsafePerformIO (readMVar __template_store)

__template_store :: MVar TemplatesAndFilters
__template_store = unsafePerformIO newEmptyMVar

-- | 'HTML' content type, but more than just that.
--
--   'HTML' takes a type-level string which is
--   a filename for the template you want to use to
--   render values. Just like 'Tpl', types used with
--   the 'HTML' content type (like @User@ below)
--   must provide a 'ToObject' instance.
--
--   Example:
--
-- @
-- type UserAPI = "user" :> Get '[JSON, HTML "user.tpl"] User
--
-- userAPI :: Proxy UserAPI
-- userAPI = Proxy
--
-- data User = User { name :: String, age :: Int } deriving Generic
--
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
--
-- /IMPORTANT/: it XSS-sanitizes every bit of text in the 'Object'
-- passed to the template.
data HTML (file :: Symbol)

-- | @text/html;charset=utf-8@
instance Accept (HTML file) where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

-- | XSS-sanitizes data before rendering it
instance (KnownSymbol file, ToObject a) => MimeRender (HTML file) a where
  mimeRender _ val = mimeRender (Proxy :: Proxy (Tpl (HTML file) file)) $
    sanitizeObject (toObject val)

sanitizeObject :: Object -> Object
sanitizeObject = HM.fromList . map sanitizeKV . HM.toList

sanitizeKV :: (Text, Value) -> (Text, Value)
sanitizeKV (k, v) = (sanitize k, sanitizeValue v)

sanitizeValue :: Value -> Value
sanitizeValue (String s) = String (sanitize s)
sanitizeValue (Array a) = Array (V.map sanitizeValue a)
sanitizeValue (Object o) = Object (sanitizeObject o)
sanitizeValue x = x

type family Append (xs :: [k]) (ys :: [k]) :: [k] where
  Append '[]       ys = ys
  Append (x ': xs) ys = x ': Append xs ys

type family Member (x :: k) (xs :: [k]) :: Bool where
  Member x (x ': xs) = 'True
  Member x (y ': xs) = Member x xs
  Member x       '[] = 'False

-- | Collect all the template filenames of an API as a type-level
--   list of strings, by simply looking at all occurences of the
--   'Tpl' and 'HTML' combinators and keeping the filenames associated to them.
type family TemplateFiles (api :: k) :: [Symbol]
type instance TemplateFiles (a :<|> b)    = Append (TemplateFiles a) (TemplateFiles b)
type instance TemplateFiles (a :> r)      = TemplateFiles r
type instance TemplateFiles (Delete cs a) = CTFiles cs
type instance TemplateFiles (Get cs a)    = CTFiles cs
type instance TemplateFiles (Patch cs a)  = CTFiles cs
type instance TemplateFiles (Post cs a)   = CTFiles cs
type instance TemplateFiles (Put cs a)    = CTFiles cs
type instance TemplateFiles Raw           = '[]

type family CTFiles (cts :: [*]) :: [Symbol] where
  CTFiles '[]        = '[]
  CTFiles (c ': cts) = Append (CTFile c) (CTFiles cts)

type family CTFile c :: [Symbol] where
  CTFile (HTML fp)   = '[fp]
  CTFile (Tpl ct fp) = '[fp]
  CTFile a           = '[]

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

-- A data type that holds both the compiled templates and
-- any passed-in custom filters
data TemplatesAndFilters = TemplatesAndFilters {
                                  _templates :: Templates
                                , _filters   :: HashMap Text Term
                                }

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
