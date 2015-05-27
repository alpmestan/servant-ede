{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Servant.HTML.EDE
  ( Tpl
  , loadTemplates
  , Errors(..)
  , Validated(..)
  , Templates
  ) where

import Control.Applicative
import Control.Monad.IO.Class
import Data.ByteString.Lazy.Char8 (pack)
import Data.Foldable
import Data.HashMap.Strict
import Data.Semigroup
import Data.Traversable
import Data.Text.Lazy.Encoding (encodeUtf8)
import GHC.TypeLits
import Network.HTTP.Types
import Network.Wai
import Servant
import Servant.Server.Internal
import Servant.HTML.EDE.Internal
import System.FilePath
import Text.EDE

import qualified Data.HashMap.Strict as HM

data Tpl (tplfile :: Symbol)

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

type family TemplateFiles (api :: k) :: [Symbol] where
  TemplateFiles (a :<|> b) = Append (TemplateFiles a) (TemplateFiles b)
  TemplateFiles (a :> r)   = TemplateFiles r
  TemplateFiles (Tpl f)    = '[f]
  TemplateFiles a          = '[]

templates :: Proxy api -> Proxy (TemplateFiles api)
templates Proxy = Proxy

templateFiles :: Reify (TemplateFiles api) => Proxy api -> [FilePath]
templateFiles = reify . templates

-- TODO: Hide the constructor.
newtype Templates = Templates (HashMap String Template)
  deriving Eq

instance Semigroup Templates where
  Templates a <> Templates b = Templates (a <> b)

instance Monoid Templates where
  mempty = Templates mempty

  Templates a `mappend` Templates b = Templates (a `mappend` b)

tpl :: FilePath -> Template -> Templates
tpl fp t = Templates $ HM.singleton fp t

type TemplateError = (FilePath, String)
type Errors = [TemplateError]

err :: Show a => FilePath -> a -> Errors
err fp d = [(fp, show d)]

loadTemplates :: (Reify (TemplateFiles api), Applicative m, MonadIO m)
              => Proxy api
              -> FilePath   -- ^ root directory for the templates
              -> m (Validated Errors Templates)
loadTemplates proxy templatedir =
  fmap (fmap fold) . runValidateT $
    traverse (processFile templatedir) files

  where files :: [FilePath]
        files = templateFiles proxy

processFile :: MonadIO m => FilePath -> FilePath -> ValidateT Errors m Templates
processFile d fp = validate . liftIO $ parseFile' (d </> fp)

  where parseFile' f = fmap validateResult (parseFile f)
        validateResult (Success t) = OK (tpl fp t)
        validateResult (Failure d) = NotOK (err fp d)
