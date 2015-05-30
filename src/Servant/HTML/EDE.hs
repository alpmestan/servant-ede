{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module:      Servant.HTML.EDE
-- Copyright:   (c) 2015 Alp Mestanogullari
-- License:     BSD3
-- Maintainer:  Alp Mestanogullari <alpmestan@gmail.com>
-- Stability:   experimental
--
-- Combinators for rendering <http://hackage.haskell.org/package/ede ede>
-- templates in servant web applications.
module Servant.HTML.EDE (
    -- * Introduction
    -- $intro

    -- * Explicitly binding data to a template
    -- $explicit

    -- * Template rendering as a content-type
    -- $contenttype
    
    -- * Reference

    -- ** 'Tpl' combinator, for explicit data binding
    Tpl
  
  , -- ** 'HTML' content type, for serializing data types to HTML
    HTML
  , ToObject(..)

  , -- ** Loading templates
    loadTemplates
  , TemplateError
  , Errors

  , -- ** Helpers
    TemplateFiles
  , Reify(..)

  , -- * Global template store
    -- $templatestore
  ) where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Data.Aeson
import Data.Foldable
import Data.HashMap.Strict
import Data.Traversable
import Servant
import Servant.HTML.EDE.Internal

-- | Same as 'loadTemplates', except that it initializes a global
--   template store (i.e a 'Templates' value) and fills it with
--   the resulting compiled templates if all of them are compiled
--   successfully. If that's not the case, the global template store
--   (under an 'MVar') is left empty.
--
--   /IMPORTANT/: Must /always/ be called before starting your /servant/ application.
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

-- $intro
--
-- The <http://hackage.haskell.org/package/ede ede> library provides
-- a reasonably good template engine. This package explores
-- a smooth integration of /ede/ into <http://haskell-servant.github.io/ servant>
-- through two combinators.

-- $explicit
--
-- The first combinator is 'Tpl', which is used for generating web pages
-- from /ede/ templates by explicitly returning an 'Object'
-- (which is a synonym for @'HashMap' 'Text' 'Value'@, i.e a /JSON object/)
-- from the handler which will then be rendered against a given template.
--
-- @
-- -- we want our template file, index.html,
-- -- to be rendered under /index
-- type API = "index" :> 'Tpl' "index.html"
--
-- api :: 'Proxy' API
-- api = Proxy
--
-- server :: 'Server' API
-- server = return indexData
--
--   where indexData :: 'Object'
--         indexData =
--           HM.fromList [ ("company_name", "Foo Inc.")
--                       , ("ceo", "Bar Baz")
--                       ]
--
-- main :: IO ()
-- main = do
--   -- this tells the library to look for index.html
--   -- under the ./templates directory
--   'loadTemplates' api "./templates"
--   run 8080 server
-- @
--
-- Note that if your template doesn't need any data, you can just
-- return the empty 'Object', with "Data.Monoid"\'s 'mempty'.
--
-- The call to 'loadTemplates' is mandatory. It loads and compiles all the
-- templates used in your API and puts them in a global \"compiled template store\".
-- I'm not really satisfied with this, see the /Global template store/ section
-- for more on this topic.

-- $contenttype
--
-- The other way to use this package is reminiscent of how you can render HTML
-- with <http://hackage.haskell.org/package/servant-blaze servant-blaze> or
-- its cousin <http://hackage.haskell.org/package/servant-lucid servant-lucid>.
-- Indeed, this package provides an 'HTML' content type just like the two
-- aforementionned libraries. However, unlike in these packages, this 'HTML'
-- type is parametrised by a type-level string meant to be the name of the
-- template file (or path to the template file starting from a \"root\" directory
-- of templates).
--
-- In the same way that /servant/'s standard 'JSON' combinator
-- carries the precise way in which we encode haskell values to JSON,
-- in addition to representing the @application/json@ content type,
-- 'HTML' carries the template used to render values of a given type.
--
-- If we wanted to have a @/user@ endpoint accessible in JSON or HTML,
-- returning a user, we could write:
--
-- @
-- type UserAPI = "user" :> 'Get' '['JSON', 'HTML' "user.tpl"] User
--
-- userAPI :: Proxy UserAPI
-- userAPI = Proxy
-- @
--
-- How, then, can /servant/ know how to marshall @User@ to the template
-- in order to render it? Simple, you just have to provide an instance of
-- the following 'ToObject' class:
--
-- @
-- class ToObject a where
--   toObject :: a -> 'Object'
-- @
--
-- If our @User@ data type is:
--
-- > data User = User { name :: String, age :: Int }
--
-- we can simply do, using functions from "Text.EDE":
--
-- @
-- instance ToObject User where
--   toObject u =
--     fromPairs [ "name" .= name u
--               , "age"  .= age u
--               ]
-- @
--
-- However, this is actually not necessary. This library provides can
-- derive the 'ToObject' instance for you as long as your data type
-- derives the "GHC.Generics.Generic" class, which can be done by specifying
-- @{-# LANGUAGE DeriveGeneric #-}@ at the top of your module, adding
-- @import GHC.Generics@ and by changing the @User@ data type declaration to:
--
-- > data User = User { name :: String, age :: Int } deriving Generic
--
-- You can then simply write:
--
-- > instance ToObject User
--
-- and the library will figure out how to encode @User@ to JSON for you.
-- /IMPORTANT/: This only works with data types with a single record and
-- field selectors.
--
-- Now we can put this all to work with a simple webservice:
--
-- @
-- server :: Server UserAPI
-- server = return (User "lambdabot" 31)
--
-- main :: IO ()
-- main = do
--   loadTemplates userAPI "./templates"
--   run 8082 (serve userAPI server)
-- @
--
-- Again, the call to 'loadTemplates' is mandatory because the 'HTML'
-- content type relies on having its hands on already-compiled templates.
--
-- You can now write a @user.tpl@ template under the @./templates@ directory
-- using any of <http://hackage.haskell.org/package/ede ede>'s constructs,
-- assuming that a @name@ string variable and an @age@ int variable are in scope.

-- $templatestore
--
-- Why have a global template store? Well, while for 'Tpl' we can run arbitrary code
-- in the handlers, take arguments and what not, that's not the case when writing the
-- 'MimeRender' instance for 'HTML'.
--
-- All we have is a value of some type and we have to pull a compiled template out of
-- thin air. That's why we use a global 'MVar'-protected template store indexed by filename.
-- It's filled once at the beginning when you call 'loadTemplates' and is then only accessed
-- in a /read-only/ fashion. I would be interested in hearing about any suggestion,
-- improvement or replacement for this. If you have an idea, feel free to shoot me an
-- email at the address specified in the cabal description.
