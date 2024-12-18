{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Deriving.Wrappers (
  FocusedOn,
  MappedTo (..),
  FieldType,
  WithKey (..),
  Keyed (..),
  Merged (..),
  Symbols (..),
  SomeField (..),
  Merging (..),
  Ignored (..),
  SameRepAs (..),
  coerceViaRep,
  coerceFromRep,
  Coerced (..),
  Structural (..),
) where

import Data.Aeson qualified as J
import Data.Aeson.Key qualified as J
import Data.Aeson.KeyMap qualified as KM
import Data.Coerce
import Data.Convert.Structural
import Data.Kind
import Data.Proxy
import Data.String
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics
import GHC.Records
import GHC.TypeLits

newtype MappedTo a (ss :: [(Symbol, Symbol)]) = MappedTo {runMappedTo :: a}
  deriving (Generic)
  deriving newtype (Read, Show, Eq, Ord)

type FocusedOn a ss = MappedTo a (IdTuples ss)

newtype Ignored a = Ignored {runIgnore :: a}
  deriving (Read, Show, Eq, Ord, Generic)

instance J.ToJSON (Ignored v) where
  toJSON = const $ J.Object mempty

type family IdTuples (ss :: [Symbol]) :: [(Symbol, Symbol)] where
  IdTuples '[] = '[]
  IdTuples (s ': ss) = '(s, s) ': IdTuples ss

type NoFieldMsg sym ty =
  'Text "Field \""
    ':<>: 'Text sym
    ':<>: 'Text "\" not found in type `"
    ':<>: 'ShowType ty
    ':<>: 'Text "'."

type FieldType' sym t = FieldType'_Aux sym t (FieldType sym t)

type family FieldType'_Aux sym t m where
  FieldType'_Aux sym t 'Nothing = TypeError (NoFieldMsg sym t)
  FieldType'_Aux _ _ ('Just ty) = ty
  FieldType'_Aux _ t _ =
    TypeError
      ( 'Text "Unknown error; possibly due to the Generic instance for "
          ':<>: 'ShowType t
          ':<>: 'Text "."
      )

type FieldType sym t = GFieldType sym (Rep t)

type family (l :: Maybe k) <|> (r :: Maybe k) :: Maybe k where
  'Nothing <|> r = r
  'Just l <|> _ = 'Just l

type family GTypeOf f :: Maybe Type where
  GTypeOf (Rec0 i) = 'Just i
  GTypeOf _ = 'Nothing

type family GFieldType sym (f :: Type -> Type) :: Maybe Type where
  GFieldType sym (S1 ('MetaSel ('Just sym) _ _ _) m) = GTypeOf m
  GFieldType sym (l :*: r) = GFieldType sym l <|> GFieldType sym r
  GFieldType sym (D1 _ m) = GFieldType sym m
  GFieldType sym (C1 _ m) = GFieldType sym m
  GFieldType sym _ = 'Nothing

type family Fsts (ts :: [(a, b)]) :: [a] where
  Fsts '[] = '[]
  Fsts ('(a, _) ': as) = a ': Fsts as

type family AllFieldsSats c ss t :: Constraint where
  AllFieldsSats _ '[] _ = ()
  AllFieldsSats c (s ': ss) t =
    ( AllFieldsSats_Aux c s t (FieldType s t)
    , AllFieldsSats c ss t
    )

type family AllFieldsSats_Aux c s t m :: Constraint where
  AllFieldsSats_Aux _ s t 'Nothing = TypeError (NoFieldMsg s t)
  AllFieldsSats_Aux c s t ('Just fty) =
    (c fty, HasField s t fty)

data SomeField c a where
  SomeField ::
    (HasField s a (FieldType' s a), c (FieldType' s a)) =>
    Proxy c ->
    Proxy s ->
    SomeField c a

class Symbols c a (ss :: [(Symbol, Symbol)]) where
  symbols :: p c -> q a -> r ss -> [(SomeField c a, Text)]

instance Symbols c a '[] where
  symbols _ _ _ = []

instance
  ( HasField s a (FieldType' s a)
  , c (FieldType' s a)
  , KnownSymbol t
  , Symbols c a ss
  ) =>
  Symbols c a ('(s, t) ': ss)
  where
  symbols _ _ _ =
    let k = SomeField (Proxy @c) (Proxy @s)
        v = T.pack (symbolVal $ Proxy @t)
     in (k, v) : symbols (Proxy @c) (Proxy @a) (Proxy @ss)

instance
  (Symbols J.ToJSON a ss, Generic a) =>
  J.ToJSON (a `MappedTo` ss)
  where
  toJSON (MappedTo a) =
    J.object
      [ J.fromText name J..= getField @s a
      | (SomeField _ (Proxy :: Proxy s), name) <-
          symbols (Proxy @J.ToJSON) (Proxy @a) (Proxy @ss)
      ]

{- | 型 @a@ の JSON/CSV に、キー @k@、値 @b@ のフィールドを追加したもの。
   JSON の場合は field として追加され、CSV の場合は左にラベルの列が入る。
-}
data WithKey (k :: Symbol) a b = WithKey
  { payload :: a
  , addition :: b
  }
  deriving (Read, Show, Eq, Ord, Generic)

instance (J.ToJSON a, J.ToJSON b, KnownSymbol k) => J.ToJSON (WithKey k a b) where
  toJSON WithKey {..} =
    let key = fromString (symbolVal (Proxy @k))
     in case J.toJSON payload of
          J.Object dic ->
            J.Object $ KM.insert key (J.toJSON addition) dic
          _ -> error "Underlying type is not a record"

data Merged a b = Merged {inL :: !a, inR :: !b}
  deriving (Read, Show, Eq, Ord, Generic)

infixr 9 `Merged`

instance (J.ToJSON a, J.ToJSON b) => J.ToJSON (Merged a b) where
  toJSON (Merged a b) =
    case (J.toJSON a, J.toJSON b) of
      (J.Object l, J.Object r) -> J.Object $ l <> r
      _ -> error "Both of merged values must be a object!"

data family Merging tpl

newtype instance Merging (a, b) = Merging2 {merged :: Merged a b}
  deriving (Read, Show, Eq, Ord, Generic)
  deriving newtype (J.ToJSON)

data instance Merging (a, b, c) = Merging3 {in1 :: !a, in2 :: !b, in3 :: !c}
  deriving (Read, Show, Eq, Ord, Generic)

instance (J.ToJSON a, J.ToJSON b, J.ToJSON c) => J.ToJSON (Merging (a, b, c)) where
  toJSON (Merging3 a b c) =
    case (J.toJSON a, J.toJSON b, J.toJSON c) of
      (J.Object l, J.Object r, J.Object u) -> J.Object $ l <> r <> u
      _ -> error "Both of merged values must be a object!"

newtype SameRepAs a b = SameRepAs {runSameRepAs :: a}

instance
  ( J.ToJSON b
  , Generic a
  , Generic b
  , Coercible (Rep a ()) (Rep b ())
  ) =>
  J.ToJSON (a `SameRepAs` b)
  where
  toJSON = J.toJSON . coerceViaRep

instance
  ( J.FromJSON b
  , Generic a
  , Generic b
  , Coercible (Rep a ()) (Rep b ())
  ) =>
  J.FromJSON (a `SameRepAs` b)
  where
  parseJSON = fmap coerceFromRep . J.parseJSON

coerceViaRep ::
  forall a b.
  ( Generic a
  , Generic b
  , Coercible (Rep a ()) (Rep b ())
  ) =>
  SameRepAs a b ->
  b
coerceViaRep =
  to . coerce @(Rep a ()) @(Rep b ()) . from . runSameRepAs
{-# INLINE coerceViaRep #-}

coerceFromRep ::
  forall a b.
  ( Generic a
  , Generic b
  , Coercible (Rep a ()) (Rep b ())
  ) =>
  b ->
  SameRepAs a b
coerceFromRep =
  SameRepAs . to . coerce @(Rep b ()) @(Rep a ()) . from
{-# INLINE coerceFromRep #-}

instance
  ( Generic a
  , Generic b
  , Coercible (Rep a ()) (Rep b ())
  , Semigroup b
  ) =>
  Semigroup (a `SameRepAs` b)
  where
  x <> y = coerceFromRep $ coerceViaRep x <> coerceViaRep y

instance
  ( Generic a
  , Generic b
  , Coercible (Rep a ()) (Rep b ())
  , Monoid b
  ) =>
  Monoid (a `SameRepAs` b)
  where
  mempty = coerceFromRep mempty

newtype Keyed (k :: Symbol) a = Keyed {runKeyed :: a}

instance (J.ToJSON a, KnownSymbol sym) => J.ToJSON (Keyed sym a) where
  toJSON (Keyed a) = J.object [fromString (symbolVal $ Proxy @sym) J..= a]

newtype Coerced a = Coerced {runCoerced :: a}
  deriving (Generic)

newtype Structural a = Structural {runStructural :: a}
  deriving (Generic)

instance (Coercible a b) => Convertible a (Coerced b) where
  conv = coerce

instance (GConv' a b) => Convertible a (Structural b) where
  conv = coerce @(a -> b) convertM
