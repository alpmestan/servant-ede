{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE OverloadedStrings #-}
module Servant.EDE.Internal.ToObject where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key
import qualified Data.HashMap.Strict as HashMap
import Data.Text
import GHC.Generics

-- | Turn haskell values into JSON objects.
--
-- This is the mechanism used by EDE to marshall data from Haskell
-- to the templates. The rendering is then just about feeding the
-- resulting 'Object' to a compiled 'Template'. Example:
--
-- > import Text.EDE
-- >
-- > data User = User { name :: String, age :: Int }
-- >
-- > instance ToObject User where
-- >   toObject user =
-- >     fromPairs [ "name" .= name user
-- >               , "age"  .= age user
-- >               ]
--
-- However, you're not forced to write the instance yourself for such a type.
-- Indeed, for any record type (i.e a datatype with a single constructor and
-- with field selectors) you can let @GHC.Generics@ derive the 'ToObject' instance
-- for you.
--
-- > data User = User { name :: String, age :: Int } deriving Generic
-- > instance ToObject User
--
-- This will generate an equivalent instance to the previous one.
class ToObject a where

  -- | Turn values of type @a@ into JSON 'Object's.
  --
  -- @ 
  -- -- Reminder:
  -- type Object = 'KeyMap' 'Value'
  -- @
  toObject :: a -> Object
  
  default toObject :: (Generic a, GToObject (Rep a)) => a -> Object
  toObject = genericToObject

instance ToObject (HashMap.HashMap Text Value) where
  toObject hm = KeyMap.fromList [(Key.fromText k, v) | (k,v) <- HashMap.toList hm]

instance ToObject (KeyMap.KeyMap Value) where
  toObject = id

class GToObject f where
  gtoObject :: f a -> Object

instance GToObject V1 where
  gtoObject _ = mempty

instance GToObject U1 where
  gtoObject U1 = mempty

instance (GToObject f, GToObject g)
      => GToObject (f :*: g) where
  gtoObject (f :*: g) = gtoObject f <> gtoObject g

instance GToObject a => GToObject (M1 D d a) where
  gtoObject (M1 x) = gtoObject x

instance GToObject a => GToObject (M1 C c a) where
  gtoObject (M1 x) = gtoObject x

instance (Selector s, ToJSON a) => GToObject (M1 S s (K1 r a)) where
  gtoObject s@(M1 (K1 x)) = KeyMap.fromList [(fieldname, value)]
    where fieldname = Key.fromText (pack (selName s))
          value     = toJSON x

genericToObject :: (Generic a, GToObject (Rep a)) => a -> Object
genericToObject = gtoObject . from
