{-# LANGUAGE CPP                        #-}
#if __GLASGOW_HASKELL__ < 900
{-# LANGUAGE AllowAmbiguousTypes        #-}
#endif
#if __GLASGOW_HASKELL__ < 904
{-# LANGUAGE ConstraintKinds            #-}
#endif
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneKindSignatures   #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Servant.EDE
-- Copyright   :  (c) Alp Mestanogullari 2015
-- Maintainer  :  sandy.maguire@tweag.io
-- Stability   :  stable
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
  , toEdeObject

  , serveWithContextAndTemplates
  , unsafeLoadTemplates
  , LoadedTemplates
  , TemplateFiles(..)
  , ReifiedTemplate(..)
  , instantiate
  , Trivial
  , ContentTemplateFiles(..)
  , HasTemplate(..)
  ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import Control.Monad.IO.Class
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Map.Monoidal as MM
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Set as S
import Data.Set (Set)
import Data.Traversable (for)
#if __GLASGOW_HASKELL__ >= 904
import GHC.Base (withDict)
#else
import Unsafe.Coerce (unsafeCoerce)
#endif
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
import Data.ByteString.Lazy (ByteString)
import Servant.Server

import qualified Data.HashMap.Strict as HM
import qualified Data.Vector         as V

-- | Special class for safely passing IO-loaded templates into type-level
-- combinators. Instances of 'LoadedTemplates' are only provided by
-- 'serveWithContextAndTemplates' and 'unsafeLoadTemplates'.
--
-- @since 1.0.0.0
class LoadedTemplates where
  loadedTemplates :: TemplatesAndFilters Trivial

#if __GLASGOW_HASKELL__ < 904
-- | Compatibility shim for @withDict@, which was only introduced in GHC 9.4.
--
-- This implements the standard single-method-class reflection trick: a class
-- with a single method and no superclasses is represented at runtime exactly by
-- that method's value, so we can reinterpret the method as the class dictionary.
-- This is safe for 'LoadedTemplates', whose sole method is 'loadedTemplates'.
newtype Gift c r = Gift (c => r)

withDict :: forall c meth r. meth -> (c => r) -> r
withDict meth k = unsafeCoerce (Gift k :: Gift c r) meth
#endif

-- @since 0.6
type Filter = (Text,Term)

-- @since 1.0.0.0
serveWithContextAndTemplates
    :: forall api ctx global
     . ( LoadedTemplates => HasServer api ctx
       , ServerContext ctx
       , TemplateFiles Trivial api
       , ToObject global
       )
    => [Filter]
    -> FilePath
    -> global
    -- ^ A global object that is available inside every templates. If the names
    -- in this object overlap names in the template-specific object, the
    -- template's keys will shadow the global object's.
    -> Proxy api
    -> Context ctx
    -> ServerT api Handler
    -> IO (Application)
serveWithContextAndTemplates fs dir global api ctx server = do
  r <-
    unsafeLoadTemplates (Proxy @api) fs dir global
      $ pure
      $ serveWithContext api ctx server
  case r of
    Left es ->
      error $ unlines $ do
        (fp, errs) <- M.toList es
        (fp <> ":") : do
          err <- S.toList errs
          pure $ "- " <> err
    Right a -> pure a


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
-- > main = either print pure $ unsafeLoadTemplates api "path/to/templates" $ ...
--
-- This would try to load @home.tpl@, printing any errors or performing the
-- actions given by @...@.
--
-- This function is unsafe because nothing ties the provided 'LoadedTemplates'
-- instance to the given @api@. You should prefer
-- 'serveWithContextAndTemplates' whenever possible.
--
-- @since 1.0.0.0
unsafeLoadTemplates
  :: (TemplateFiles Trivial api, MonadIO m, ToObject global)
  => Proxy api
  -> [Filter] -- ^ list of (Text,Term) pairs. Pass [] to use just the standard library
  -> FilePath -- ^ root directory for the templates
  -> global
  -> (LoadedTemplates => m r)
  -> m (Either (Map FilePath (Set String)) r)
unsafeLoadTemplates proxy fpairs dir global k = do
  let flts = fromList fpairs
  res <- liftIO $ loadTemplates' @Trivial proxy dir
  case res of
    Left errs  -> pure $ Left $ MM.getMonoidalMap errs
    Right tpls -> do
      fmap Right $ withDict @LoadedTemplates (TemplatesAndFilters tpls flts $ toObject global) k


loadTemplates'
    :: forall c api
     . (TemplateFiles c api, c ())
    => Proxy api
    -> FilePath
    -> IO (Either Errors (HashMap FilePath (ReifiedTemplate c Template)))
loadTemplates' proxy
  = fmap (eitherValidate . fmap fold)
  . runValidateT
  . for (M.elems $ reifyTemplates proxy)
  . processFile

-- | A generic template combinator, parametrized over
--   the content-type (or MIME) associated to the template.
--
--   The parameter is the content-type you want to send along with rendered
--   templates (must be an instance of 'Accept').
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
--   run 8082 =<< 'serveWithContextAndTemplates' [] "./templates" styleAPI EmptyContext server
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
--
-- @since 0.4
data Tpl (contentType :: Type)

instance Accept contentType => Accept (Tpl contentType) where
  contentType _ = contentType $ Proxy @contentType

-- | Given a content type and an type of handler output, give a path to an EDE
-- template file.
--
-- @since 1.0.0.0
class HasTemplate contentType a where
  templateFor :: Proxy contentType -> Proxy a -> FilePath


-- | Common implementation of 'mimeRender'.
doMimeRender
    :: (LoadedTemplates, ToObject a)
    => (Object -> Object)
    -- ^ Transformation on the object data before rendering.
    -> FilePath
    -> a
    -> ByteString
doMimeRender process fp
  = encodeUtf8
  . result (error . show) id
  . renderWith (filters loadedTemplates) (unReifiedTemplate $ templates loadedTemplates ! fp)
  . HM.fromList
  . fmap (first Key.toText)
  . KeyMap.toList
  . process
  . -- The object semigroup instance is left-biased, so we want to insert the
    -- global object on the right to prevent any global shadowing.
    (<> globalObj loadedTemplates)
  . toObject

instance (LoadedTemplates, HasTemplate contentType a, Accept contentType, ToObject a) => MimeRender (Tpl contentType) a where
  mimeRender _ = doMimeRender id $ templateFor (Proxy @contentType) (Proxy @a)

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
-- main = run 8082 =<< 'serveWithContextAndTemplates' [] "./templates" () userAPI NoContext server
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
--
-- @since 0.4
data HTML

-- | @text/html;charset=utf-8@
instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

-- | XSS-sanitizes data before rendering it
instance (LoadedTemplates, HasTemplate HTML a, ToObject a) => MimeRender HTML a where
  mimeRender _ = doMimeRender sanitizeObject $ templateFor (Proxy @HTML) (Proxy @a)

sanitizeObject :: Object -> Object
sanitizeObject = KeyMap.fromList . map sanitizeKV . KeyMap.toList

sanitizeKV :: (Key.Key, Value) -> (Key.Key, Value)
sanitizeKV (k, v) = (Key.fromText  . sanitize $ Key.toText k, sanitizeValue v)

sanitizeValue :: Value -> Value
sanitizeValue (String s) = String (sanitize s)
sanitizeValue (Array a) = Array (V.map sanitizeValue a)
sanitizeValue (Object o) = Object (sanitizeObject o)
sanitizeValue x = x

-- | Collect all the template filenames of an API by simply looking at all
-- occurences of the 'Tpl' and 'HTML' combinators and keeping the filenames
-- associated to them.
--
-- The @c@ parameter is of kind @'Type' -> 'Constraint'@ and can be used to
-- ensure every that every return type in your API satisfies some constraint.
-- If you don't have a need for this parameter, you can fill it in with
-- 'Trivial'.
--
-- @since 1.0.0.0
type TemplateFiles :: (Type -> Constraint) -> k -> Constraint
class TemplateFiles c api where
  reifyTemplates :: Proxy api -> Map FilePath (ReifiedTemplate c ())

instance (TemplateFiles c a, TemplateFiles c b) => TemplateFiles c (a :<|> b) where
  reifyTemplates _ = reifyTemplates (Proxy @a) <> reifyTemplates (Proxy @b)

instance (TemplateFiles c api) => TemplateFiles c (a :> api) where
  reifyTemplates _ = reifyTemplates $ Proxy @api

instance ContentTemplateFiles c contentType a => TemplateFiles c (Verb m s contentType a) where
  reifyTemplates _ = contentTemplatesFor (Proxy @contentType) (Proxy @a)

instance TemplateFiles c Raw where
  reifyTemplates _ = mempty

instance TemplateFiles c (ToServantApi a) => TemplateFiles c (NamedRoutes a) where
  reifyTemplates _ = reifyTemplates (Proxy @(ToServantApi a))

instance TemplateFiles c EmptyAPI where
  reifyTemplates _ = mempty


-- | Collect template files for a given set of content types.
--
-- @since 1.0.0.0
type ContentTemplateFiles :: (Type -> Constraint) -> [Type] -> Type -> Constraint
class ContentTemplateFiles c contentType a where
  contentTemplatesFor :: Proxy contentType -> Proxy a -> Map FilePath (ReifiedTemplate c ())

instance ContentTemplateFiles c '[] a where
  contentTemplatesFor _ _ = mempty

instance
    {-# OVERLAPPING #-}
    ( HasTemplate HTML a
    , ContentTemplateFiles c contentTypes a
    , ToObject a
    , c a
    )
      => ContentTemplateFiles c (HTML ': contentTypes) a where
  contentTemplatesFor _ pa =
    let fp = templateFor (Proxy @HTML) pa
     in M.insert fp (ReifiedTemplate (Proxy @a) fp ()) $ contentTemplatesFor (Proxy @contentTypes) pa

instance
    {-# OVERLAPPING #-}
    ( HasTemplate contentType a
    , ContentTemplateFiles c contentTypes a
    , ToObject a
    , c a
    )
      => ContentTemplateFiles c (Tpl contentType ': contentTypes) a where
  contentTemplatesFor _ pa =
    let fp = templateFor (Proxy @contentType) pa
     in M.insert fp (ReifiedTemplate (Proxy @a) fp ()) $ contentTemplatesFor (Proxy @contentTypes) pa

instance
    {-# OVERLAPPABLE #-}
    (ContentTemplateFiles c contentTypes a)
      => ContentTemplateFiles c (contentType ': contentTypes) a where
  contentTemplatesFor _ pa = contentTemplatesFor (Proxy @contentTypes) pa

-- A data type that holds both the compiled templates and
-- any passed-in custom filters
data TemplatesAndFilters c = TemplatesAndFilters
  { templates :: HashMap FilePath (ReifiedTemplate c Template)
  , filters   :: HashMap Text Term
  , globalObj :: Object
  }

-- | A trivial class that always has instances for every type. This is useful
-- when you don't need the full power of 'TemplateFiles' or 'ReifiedTemplate'.
class Trivial a
instance Trivial a

-- | A 'ReifiedTemplate' contains the filepath of the template, as well as its
-- return type, and an optional constraint @c@ that the return type is
-- guaranteed to satisfy. For example, you can generate property tests showing
-- that your templates compile and can be instantiated by letting @c
-- ~ TestableC@, where
--
-- @
-- class (Show a, Eq a, Arbitrary a) => TestableC a
-- instance (Show a, Eq a, Arbitrary a) => TestableC a
-- @
--
-- and then use 'reifyTemplates' to get a map of @'ReifiedTemplate' TestableC ()@s.
-- By subsequently pattern matching on the 'ReifiedTemplate' constructor, you
-- now have everything in scope necessary to write a quickcheck-style property
-- test.
type ReifiedTemplate :: (Type -> Constraint) -> Type -> Type
data ReifiedTemplate c x where
  ReifiedTemplate
    :: (c a, ToObject a)
    => { mt_proxy :: Proxy a
       , mt_path :: FilePath
       , unReifiedTemplate :: x
       } -> ReifiedTemplate c x

instance Functor (ReifiedTemplate c) where
  fmap f (ReifiedTemplate p fp a) = ReifiedTemplate p fp $ f a

instance Foldable (ReifiedTemplate c) where
  foldMap f (ReifiedTemplate _ _ a) = f a

instance Traversable (ReifiedTemplate c) where
  traverse f (ReifiedTemplate p fp a) = fmap (ReifiedTemplate p fp) $ f a

type Errors = MonoidalMap FilePath (Set String)

processFile
    :: FilePath
    -> ReifiedTemplate c ()
    -> ValidateT Errors IO (HashMap FilePath (ReifiedTemplate c Template))
processFile d t@(ReifiedTemplate _ fp _)
  = validate
  $ fmap
      ( either
          (NotOK . MM.singleton fp . S.singleton)
          (OK . HM.singleton fp)
      )
  $ instantiate d t


-- | Parse a 'ReifiedTemplate'. This is like 'Text.EDE.parseFile', but works
-- directly over 'ReifiedTemplate's and plays more nicely with servant-ede.
--
-- @since 1.0.0.0
instantiate
    :: FilePath
    -- ^ Template directory
    -> ReifiedTemplate c ()
    -> IO (Either String (ReifiedTemplate c Template))
instantiate d (ReifiedTemplate p fp ())
  = fmap (fmap (ReifiedTemplate p fp) . eitherResult)
  $ parseFile
  $ d </> fp

