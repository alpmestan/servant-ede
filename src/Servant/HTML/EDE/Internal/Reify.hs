{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Servant.HTML.EDE.Internal.Reify (Reify(..)) where

import Data.Proxy
import GHC.TypeLits

-- | Helper class to reify a type-level list of strings
--   into a value-level list of string. Used to turn
--   the type-level list of template file names into
--   a value-level list.
class Reify (symbols :: [Symbol]) where
  reify :: Proxy symbols -> [String]

instance Reify '[] where
  reify _ = []

instance (KnownSymbol s, Reify symbols)
      => Reify (s ': symbols) where
  reify _ = symbolVal ps : reify psymbols

    where ps = Proxy :: Proxy s
          psymbols = Proxy :: Proxy symbols
