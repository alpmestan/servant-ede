{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Servant.HTML.EDE.Internal where

import Control.Applicative
import Data.Foldable
import Data.Traversable
import Data.Functor.Compose
import Data.Semigroup
import Data.Proxy
import GHC.TypeLits

type family Append (xs :: [k]) (ys :: [k]) :: [k] where
  Append '[]       ys = ys
  Append (x ': xs) ys = x ': Append xs ys

type family Member (x :: k) (xs :: [k]) :: Bool where
  Member x (x ': xs) = 'True
  Member x (y ': xs) = Member x xs
  Member x       '[] = False

class Reify (symbols :: [Symbol]) where
  reify :: Proxy symbols -> [String]

instance Reify '[] where
  reify _ = []

instance (KnownSymbol s, Reify symbols)
      => Reify (s ': symbols) where
  reify _ = symbolVal ps : reify psymbols

    where ps = Proxy :: Proxy s
          psymbols = Proxy :: Proxy symbols

--
data Validated e a = OK a | NotOK e
  deriving (Eq, Show)

instance Functor (Validated e) where
  fmap f (OK x)    = OK (f x)
  fmap _ (NotOK e) = NotOK e

instance Semigroup e => Applicative (Validated e) where
  pure x = OK x

  OK f    <*> OK x     = OK (f x)
  OK f    <*> NotOK e  = NotOK e
  NotOK e <*> OK x     = NotOK e
  NotOK e <*> NotOK e' = NotOK (e <> e')

instance Foldable (Validated e) where
  foldMap f (OK x)    = f x
  foldMap f (NotOK _) = mempty

instance Traversable (Validated e) where
  traverse f (OK x)    = fmap OK (f x)
  traverse f (NotOK x) = pure (NotOK x)

instance (Semigroup e, Semigroup a) => Semigroup (Validated e a) where
  NotOK e <> NotOK e' = NotOK (e <> e')
  NotOK e <> OK a     = NotOK e
  OK a    <> OK b     = OK (a <> b)
  OK a    <> NotOK e  = NotOK e

validateEither :: Validated e a -> Either e a
validateEither (OK x)    = Right x
validateEither (NotOK e) = Left e

eitherValidate :: Either e a -> Validated e a
eitherValidate (Left e)  = NotOK e
eitherValidate (Right x) = OK x

ok :: Applicative m => a -> ValidateT e m a
ok = VT . pure . OK

no :: Applicative m => e -> ValidateT e m a
no = VT . pure . NotOK

validated :: (e -> r) -> (a -> r) -> Validated e a -> r
validated f _ (NotOK e) = f e
validated _ f (OK x)    = f x

newtype ValidateT e m a = VT
  { runValidateT :: m (Validated e a) }

validate :: m (Validated e a) -> ValidateT e m a
validate = VT

instance Functor m => Functor (ValidateT e m) where
  fmap f (VT m) = VT $ fmap (fmap f) m

instance (Applicative m, Semigroup e) => Applicative (ValidateT e m) where
  pure = VT . pure . pure

  VT f <*> VT x = VT . getCompose $ Compose f <*> Compose x

instance (Applicative m, Semigroup e, Semigroup a) => Semigroup (ValidateT e m a) where
  VT a <> VT b = VT $ (<>) <$> a <*> b














