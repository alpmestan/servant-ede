{-# LANGUAGE CPP #-}
module Servant.EDE.Internal.Validate where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
import Data.Foldable
import Data.Traversable
#endif

import Data.Functor.Compose

data Validated e a = OK a | NotOK e
  deriving (Eq, Show)

instance Functor (Validated e) where
  fmap f (OK x)    = OK (f x)
  fmap _ (NotOK e) = NotOK e

instance Semigroup e => Applicative (Validated e) where
  pure x = OK x

  OK f    <*> OK x     = OK (f x)
  OK _    <*> NotOK e  = NotOK e
  NotOK e <*> OK _     = NotOK e
  NotOK e <*> NotOK e' = NotOK (e <> e')

instance Foldable (Validated e) where
  foldMap f (OK x)    = f x
  foldMap _ (NotOK _) = mempty

instance Traversable (Validated e) where
  traverse f (OK x)    = fmap OK (f x)
  traverse _ (NotOK x) = pure (NotOK x)

instance (Semigroup e, Semigroup a) => Semigroup (Validated e a) where
  NotOK e <> NotOK e' = NotOK (e <> e')
  NotOK e <> OK _     = NotOK e
  OK a    <> OK b     = OK (a <> b)
  OK _    <> NotOK e  = NotOK e

validateEither :: Either e a -> Validated e a
validateEither (Left e)  = NotOK e
validateEither (Right x) = OK x

eitherValidate :: Validated e a -> Either e a
eitherValidate (OK x)    = Right x
eitherValidate (NotOK e) = Left e

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
