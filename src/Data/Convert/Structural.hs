{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Data.Convert.Structural (Convertible (..), GConv', GConvertible, convertM) where

import Data.Bifunctor.Flip
import Data.Coerce
import Data.Tagged
import GHC.Generics

type GConv' a b = (Generic a, Generic b, GConvertible (Rep a) (Rep b))

convertM ::
  (GConv' a b) =>
  a ->
  b
convertM = to . gconv . from

class GConvertible f g where
  gconv :: f () -> g ()

class Convertible a b where
  conv :: a -> b
  default conv ::
    (Generic a, Generic b, GConvertible (Rep a) (Rep b)) =>
    a ->
    b
  conv = to . gconv . from

deriving anyclass instance
  {-# OVERLAPPING #-}
  (Convertible a b) =>
  Convertible (Maybe a) (Maybe b)

instance {-# OVERLAPPING #-} (Convertible a b) => Convertible (Tagged s a) (Tagged t b)

instance
  {-# OVERLAPPING #-}
  (Convertible (p b a) (q d c)) =>
  Convertible (Flip p a b) (Flip q c d)

instance Convertible Double Double where
  conv = id

instance Convertible Bool Bool where
  conv = id

instance
  {-# OVERLAPPABLE #-}
  (Convertible a b) =>
  GConvertible (K1 i a) (K1 i b)
  where
  gconv = coerce @(a -> b) conv

instance
  (GConvertible f f', GConvertible g g') =>
  GConvertible (f :*: g) (f' :*: g')
  where
  gconv (f :*: g) = gconv f :*: gconv g

instance
  (GConvertible f f', GConvertible g g') =>
  GConvertible (f :+: g) (f' :+: g')
  where
  gconv (L1 f) = L1 $ gconv f
  gconv (R1 g) = R1 $ gconv g

instance (GConvertible f f') => GConvertible (M1 i c f) (M1 i c f') where
  gconv = coerce @(f () -> f' ()) gconv

instance GConvertible U1 U1 where
  gconv = id

instance GConvertible V1 a where
  gconv = \case {}
