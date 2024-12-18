{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Coerce.Structural (SCoercible (..), GCoercible) where

import Data.Bifunctor.Flip
import Data.Coerce
import Data.Kind
import Data.Tagged
import GHC.Generics
import Unsafe.Coerce

class
  (GCoerce (Rep a) (Rep b), Generic a, Generic b) =>
  GCoercible a b

instance
  (GCoerce (Rep a) (Rep b), Generic a, Generic b) =>
  GCoercible a b

{- | GHC の既存のシステムでは推論出来ないが、
   構造上表現が一致するので Coercion が可能な型
-}
class SCoercible a b where
  coerceStruct :: a -> b
  default coerceStruct :: (GCoercible a b) => a -> b
  coerceStruct = unsafeCoerce

instance {-# OVERLAPPABLE #-} (Coercible a b) => SCoercible a b where
  coerceStruct = coerce

instance {-# OVERLAPPING #-} (SCoercible a b) => SCoercible (Tagged s a) (Tagged t b)

instance
  {-# OVERLAPPING #-}
  (SCoercible (p b a) (q d c)) =>
  SCoercible (Flip p a b) (Flip q c d)

class GCoerce (f :: Type -> Type) (g :: Type -> Type)

instance (SCoercible a b) => GCoerce (K1 i a) (K1 i b)

instance
  (GCoerce f f', GCoerce g g') =>
  GCoerce (f :*: g) (f' :*: g')

instance
  (GCoerce f f', GCoerce g g') =>
  GCoerce (f :+: g) (f' :+: g')

instance
  (GCoerce f g) =>
  GCoerce (M1 i c f) (M1 i c g)

instance GCoerce U1 U1

instance GCoerce V1 V1
