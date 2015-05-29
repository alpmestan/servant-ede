{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Servant.HTML.EDE
  ( -- * 'Tpl' combinator, for argument-less templates
    Tpl
  
  , -- * 'HTML' content type, for templates with arguments marshalled with 'ToObject'
    HTML
  , ToObject(..)

  , -- * Loading templates
    loadTemplates
  , loadTemplates_
  , TemplateError
  , Errors
  , Templates

  , -- * Helpers
    TemplateFiles
  , Reify(..)
  ) where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Data.Foldable
import Data.HashMap.Strict
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Traversable
import GHC.TypeLits
import Servant
import Servant.HTML.EDE.Internal
import System.IO.Unsafe (unsafePerformIO)
import Text.EDE

-- | Automatically load and compile all the templates used by an /API/.
--
--   Returns errors with 'Left' if any of the templates can't be compiled.
--   If they all get compiled, returns an opaque 'Templates' value which can basically
--   be seen as a store of compiled templates. This is the value you have to specify
--   for the handlers for endpoints ending in 'Tpl'.
--
--   /Note/: If you have @'Tpl' "index.tpl"@ in your API and call this function like this:
--
--   > loadTemplates api "/path/to/templates"
--
--   then this function will try to load and compile @\/path\/to\/templates\/index.tpl@.
loadTemplates :: (Reify (TemplateFiles api), Applicative m, MonadIO m)
              => Proxy api
              -> FilePath   -- ^ root directory for the templates
              -> m (Either Errors Templates)
loadTemplates proxy templatedir =
  fmap (eitherValidate . fmap fold) . runValidateT $
    traverse (processFile templatedir) files

  where files :: [FilePath]
        files = templateFiles proxy

-- * Content-Type stuffs

__template_store :: MVar Templates
__template_store = unsafePerformIO newEmptyMVar

-- | Same as 'loadTemplates', except that it initializes a global
--   template store (i.e a 'Templates' value) and fills it with
--   the resulting compiled templates if all of them are compiled
--   successfully. If that's not the case, the global template store
--   (under an 'MVar') is left empty.
--
--   /IMPORTANT/: Must be called before starting your /servant/ application,
--   if you use the 'HTML' content type from this package.
loadTemplates_ :: (Reify (TemplateFiles api), Applicative m, MonadIO m)
               => Proxy api
               -> FilePath -- ^ root directory for the templates
               -> m Errors
loadTemplates_ proxy dir = do
  res <- loadTemplates proxy dir
  case res of
    Left errs  -> return errs
    Right tpls -> do
      liftIO $ putMVar __template_store tpls
      return []

instance (KnownSymbol tplfile, ToObject a)
      => MimeRender (HTML tplfile) a where
  mimeRender _ val = encodeUtf8 . result (error . show) id $
    render tpl (toObject val)

    where tpl = tmap ! filename
          filename = symbolVal (Proxy :: Proxy tplfile)
          tmap = templateMap $ unsafePerformIO (readMVar __template_store)
