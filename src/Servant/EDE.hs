{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}

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
-- - 'HTML' lets you render the template with that name against the data
--   returned by a request handler using the @text\/html;charset=utf-8@ MIME
--   type, XSS-sanitizing the said data along the way. See 'HTML' for an
--   example.
--
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
  , LoadedTemplates
  , TemplateFiles(..)
  , HasTemplate(..)
  , Templates
  , Errors
  , TemplateError
  ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
import Data.Traversable (traverse)
#endif

import GHC.Base (withDict)
import Control.Monad.IO.Class
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key
import Data.Aeson (Object, Value(..))
import Data.Bifunctor (first)
import Data.Foldable (fold)
import Data.Kind
import Data.HashMap.Strict (HashMap, (!),fromList)
import Data.Proxy
import Data.Text (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Network.HTTP.Media hiding (Accept)
import Servant.API
import Servant.EDE.Internal.ToObject
import Servant.EDE.Internal.Validate
import System.FilePath
import Text.EDE
import Text.EDE.Filters (Term)
import Text.HTML.SanitizeXSS

import qualified Data.HashMap.Strict as HM
import qualified Data.Vector         as V

-- | Special class for safely passing IO-loaded templates into type-level
-- combinators. Instances of 'LoadedTemplates' are only provided by
-- 'loadTemplates'.
class LoadedTemplates where
  loadedTemplates :: TemplatesAndFilters

type Filter = (Text,Term)
-- | This function initializes a global template store (i.e a 'Templates' value)
-- and fills it with the resulting compiled templates if all of them are
-- compiled successfully. If that's not the case, this function returns the
-- errors.
--
-- Example:
--
-- > instance HasTemplate HTML HomeData where
-- >   templateFor _ _ = "home.tpl"
-- >
-- > type API = Get '[HTML] HomeData
-- >
-- > api :: Proxy API
-- > api = Proxy
-- >
-- > main :: IO ()
-- > main = either print pure $ loadTemplates api "path/to/templates" $ ...
--
-- This would try to load @home.tpl@, printing any errors or performing the
-- actions given by @...@.
loadTemplates :: (TemplateFiles api, Applicative m, MonadIO m)
              => Proxy api
              -> [Filter] -- ^ list of (Text,Term) pairs. Pass [] to use just the standard library
              -> FilePath -- ^ root directory for the templates
              -> (LoadedTemplates => m r)
              -> m (Either Errors r)
loadTemplates proxy fpairs dir k = do
  let flts = fromList fpairs
  res <- loadTemplates' proxy dir
  case res of
    Left errs  -> pure $ Left errs
    Right tpls -> do
      fmap Right $ withDict @LoadedTemplates (TemplatesAndFilters tpls flts) k

loadTemplates' :: (TemplateFiles api, Applicative m, MonadIO m)
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
-- type StyleAPI = "style.css" :> Get '[Tpl CSS] CSSData
--
-- styleAPI :: Proxy StyleAPI
-- styleAPI = Proxy
--
-- data CSSData = CSSData
--   { darken :: Bool
--   , pageWidth :: Int
--   } deriving Generic
--
-- instance HasTEmplate CSSData where
--   templateFor _ _ = "style.tpl"
--
-- instance ToObject CSSData
--
-- server :: Server API
-- server = -- produce a CSSData value depending on whatever is relevant...
--
-- main :: IO ()
-- main = do
--   loadTemplates styleAPI "./templates" $ run 8082 (serve styleAPI server)
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
data Tpl (ct :: Type)

instance Accept ct => Accept (Tpl ct) where
  contentType _ = contentType ctproxy
    where ctproxy = Proxy :: Proxy ct

class HasTemplate ct a where
  templateFor :: Proxy ct -> Proxy a -> FilePath

instance (LoadedTemplates, HasTemplate ct a, Accept ct, ToObject a) => MimeRender (Tpl ct) a where
  mimeRender _ val =
    let tmap = templateMap $ _templates loadedTemplates
        flts = _filters loadedTemplates
        mkObject = fromList . map (first Key.toText) . KeyMap.toList . toObject
     in encodeUtf8 . result (error . show) id $
          renderWith flts (tmap ! templateFor (Proxy @ct) (Proxy @a)) (mkObject val)

-- | 'HTML' content type, but more than just that.
--
--   Just like 'Tpl', types used with the 'HTML' content type (like @User@
--   below) must provide 'ToObject' and 'HasTemplate' instances. Unlike 'Tpl',
--   this type performs automatic escaping of HTML values to prevent XSS.
--
--   Example:
--
-- @
-- type UserAPI = "user" :> Get '[JSON, HTML] User
--
-- instance HasTemplate HTML User where
--   templateFor _ _ = "user.tpl"
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
-- main = either print pure $ loadTemplates userAPI "./templates" $ run 8082 (serve userAPI server)
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
data HTML

-- | @text/html;charset=utf-8@
instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

-- | XSS-sanitizes data before rendering it
instance (LoadedTemplates, HasTemplate HTML a, ToObject a) => MimeRender HTML a where
  mimeRender _ val =
    let tmap = templateMap $ _templates loadedTemplates
        flts = _filters loadedTemplates
        mkObject = fromList . map (first Key.toText) . KeyMap.toList . sanitizeObject . toObject
     in encodeUtf8 . result (error . show) id $
          renderWith flts (tmap ! templateFor (Proxy @HTML) (Proxy @a)) (mkObject val)

sanitizeObject :: Object -> Object
sanitizeObject = KeyMap.fromList . map sanitizeKV . KeyMap.toList

sanitizeKV :: (Key.Key, Value) -> (Key.Key, Value)
sanitizeKV (k, v) = (Key.fromText  . sanitize $ Key.toText k, sanitizeValue v)

sanitizeValue :: Value -> Value
sanitizeValue (String s) = String (sanitize s)
sanitizeValue (Array a) = Array (V.map sanitizeValue a)
sanitizeValue (Object o) = Object (sanitizeObject o)
sanitizeValue x = x

-- -- | Collect all the template filenames of an API as a type-level
-- --   list of strings, by simply looking at all occurences of the
-- --   'Tpl' and 'HTML' combinators and keeping the filenames associated to them.

type TemplateFiles :: k -> Constraint
class TemplateFiles api where
  templateFiles :: Proxy api -> [FilePath]

instance (TemplateFiles a, TemplateFiles b) => TemplateFiles (a :<|> b) where
  templateFiles _ = templateFiles (Proxy @a) <> templateFiles (Proxy @b)

instance (TemplateFiles api) => TemplateFiles (a :> api) where
  templateFiles _ = templateFiles $ Proxy @api

instance ContentTemplateFiles c a => TemplateFiles (Verb m s c a) where
  templateFiles _ = contentTemplatesFor (Proxy @c) (Proxy @a)

instance TemplateFiles Raw where
  templateFiles _ = mempty

instance TemplateFiles (ToServantApi a) => TemplateFiles (NamedRoutes a) where
  templateFiles _ = templateFiles (Proxy @(ToServantApi a))


type ContentTemplateFiles :: [Type] -> Type -> Constraint
class ContentTemplateFiles c a where
  contentTemplatesFor :: Proxy c -> Proxy a -> [FilePath]

instance ContentTemplateFiles '[] a where
  contentTemplatesFor _ _ = mempty

instance {-# OVERLAPPING #-} (HasTemplate HTML a, ContentTemplateFiles cs a) => ContentTemplateFiles (HTML ': cs) a where
  contentTemplatesFor _ pa = templateFor (Proxy @HTML) pa : contentTemplatesFor (Proxy @cs) pa

instance {-# OVERLAPPING #-} (HasTemplate c a, ContentTemplateFiles cs a) => ContentTemplateFiles (Tpl c ': cs) a where
  contentTemplatesFor _ pa = templateFor (Proxy @c) pa : contentTemplatesFor (Proxy @cs) pa

instance {-# OVERLAPPABLE #-} (ContentTemplateFiles cs a) => ContentTemplateFiles (c ': cs) a where
  contentTemplatesFor _ pa = contentTemplatesFor (Proxy @cs) pa

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
