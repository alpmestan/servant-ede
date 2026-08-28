{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

import Control.Monad
import Data.Bifunctor (first)
import Data.Either (isRight)
import Data.Foldable
import Data.HashMap.Strict (HashMap, fromList)
import Data.Map (Map)
import Data.Proxy (Proxy(..), asProxyTypeOf)
import Data.Text (Text, chunksOf)
import GHC.Generics
import Network.HTTP.Media ((//))
import Servant.API
import Servant.EDE
import System.FilePath ((</>))
import Test.Hspec
import Test.QuickCheck
import Text.EDE (parseFile, renderWith, eitherResult)
import Text.EDE.Filters ((@:),Term)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap

-- * Using 'Tpl' for rendering CSS templates

data CSS

instance Accept CSS where
  contentType _ = "text" // "css"

type StyleAPI = "style.css" :> Get '[Tpl CSS] CSSData

instance HasTemplate CSS CSSData where
  templateFor _ _ = "style.tpl"

data CSSData = CSSData
  { darken :: Bool
  , pageWidth :: Int
  } deriving (Show, Generic)

instance ToObject CSSData

instance Arbitrary CSSData where
  arbitrary = CSSData <$> arbitrary <*> arbitrary

-- * Using 'HTML' for HTML template rendering

data User = User { name :: String, age :: Int }
  deriving (Eq, Show, Generic)

instance ToObject User where

instance Arbitrary User where
  arbitrary = User <$> arbitrary <*> arbitrary

instance HasTemplate HTML User where
  templateFor _ _ = "user.tpl"

type UserAPI = "user" :> Get '[HTML] User

-- * Define an API
type API = StyleAPI :<|> UserAPI

api :: Proxy API
api = Proxy


-- * Define a constraint synonym so 'ReifiedTemplate' can guarantee every
-- template is testable.

class (Arbitrary a, Show a) => TemplateTestable a
instance (Arbitrary a, Show a) => TemplateTestable a

templates :: Map FilePath (ReifiedTemplate TemplateTestable ())
templates = reifyTemplates api


-- * Iterate over the templates, generating a property test showing that the
-- template compiles and that it can be instantiated with arbitrary data.

main :: IO ()
main = hspec $ do
  for_ templates $ \(ReifiedTemplate pa path _) ->
    beforeAll (either error pure . eitherResult =<< parseFile ("example" </> path)) $ do
      it (unwords [path, "compiles"]) $ \template ->
        -- If the templated compiled, we can try rendering it with synthetic data.
        -- The goal is to see if we can find any inputs which cause it to fail to
        -- render.
        property $ forAll arbitrary $ \a ->
          eitherResult (renderWith filters template $ toEdeObject (a `asProxyTypeOf` pa))
            `shouldSatisfy` isRight

filters :: HashMap Text Term
filters = ["toChars" @: (chunksOf 1)]

