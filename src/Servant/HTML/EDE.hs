{-# LANGUAGE FlexibleContexts #-}
module Servant.HTML.EDE
  ( -- * 'Tpl' combinator
    Tpl
  
  , -- * Loading templates
    loadTemplates
  , TemplateError
  , Errors
  , Templates

  , -- * Helpers
    TemplateFiles
  , Reify(..)
  ) where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Foldable
import Data.Traversable
import Servant
import Servant.HTML.EDE.Internal

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
