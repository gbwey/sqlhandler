{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
-- need this for DEF show instance and Writeable stuff cos calls another type family ie Or
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : HSql.Core.VinylUtils
Description : vinyl record utility methods
Copyright   : (c) Grant Weyburne, 2016
License     : BSD-3
-}
module HSql.Core.VinylUtils where

import Control.Lens (type Lens, type Lens')
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString)
import qualified Data.IntMap as IM (IntMap)
import Data.Kind
import qualified Data.Map.Strict as M (Map, empty)
import Data.Proxy
import qualified Data.Set as Set (Set, empty)
import qualified Data.Text as T
import Data.These (These (..))
import Data.Vinyl
import qualified Data.Vinyl.Functor as V (Identity (..))
import Data.Vinyl.TypeLevel (Nat (..), RIndex)
import GHC.Natural
import qualified GHC.TypeLits as GL (ErrorMessage (..), KnownSymbol, Nat, Symbol, TypeError)
import GHC.TypeNats hiding (Nat)
import Utils.One
import qualified Utils.TypeLevel as TP (Length)

-- | convert from 'Nat' to 'GL.Nat'
type FromP :: Nat -> GL.Nat
type family FromP a where
  FromP 'Z = 0
  FromP ( 'S n) = 1 + FromP n

-- | convert from 'GL.Nat' to 'Nat'
type ToP :: GL.Nat -> Nat
type family ToP a where
  ToP 0 = 'Z
  ToP n = 'S (ToP (n - 1))

-- | type synonym for a record of labelled fields
type F = Rec ElField -- :: [(GL.Symbol, Type)] -> Type

-- | type synonym for commonly used record of 'V.Identity'
type WW = Rec V.Identity -- :: [Type] -> Type

-- if you cant be bothered naming the fields manually then this will do it for you
-- useful if you want to convert to a Frame and run stats on the frame

-- | lift a ntuple of 'Type's to a promoted list of named fields for use with Rec ElField
type MakeF :: Type -> Type
type family MakeF a where
  MakeF a = Rec ElField (ToFields a)

-- | lift a promoted list of unnamed fields to named fields for use with Rec ElField
type family MakeF' as where
  MakeF' as = Rec ElField (ToFields' as)

-- max of 30

-- | hard coded list of max fields supported
type ToFields' :: [Type] -> [(GL.Symbol, Type)]
type ToFields' as = ToFields'' ["c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12", "c13", "c14", "c15", "c16", "c17", "c18", "c19", "c20", "c21", "c22", "c23", "c24", "c25", "c26", "c27", "c28", "c29", "c30"] as

-- | 'ToFields' assigns fieldnames to unnamed fields for tuples up to size 8
type ToFields :: Type -> [(GL.Symbol, Type)]
type family ToFields a = ret | ret -> a where
  ToFields (One a) = '["c1" ::: a]
  ToFields (a1, a2) = '["c1" ::: a1, "c2" ::: a2]
  ToFields (a1, a2, a3) = '["c1" ::: a1, "c2" ::: a2, "c3" ::: a3]
  ToFields (a1, a2, a3, a4) = '["c1" ::: a1, "c2" ::: a2, "c3" ::: a3, "c4" ::: a4]
  ToFields (a1, a2, a3, a4, a5) = '["c1" ::: a1, "c2" ::: a2, "c3" ::: a3, "c4" ::: a4, "c5" ::: a5]
  ToFields (a1, a2, a3, a4, a5, a6) = '["c1" ::: a1, "c2" ::: a2, "c3" ::: a3, "c4" ::: a4, "c5" ::: a5, "c6" ::: a6]
  ToFields (a1, a2, a3, a4, a5, a6, a7) = '["c1" ::: a1, "c2" ::: a2, "c3" ::: a3, "c4" ::: a4, "c5" ::: a5, "c6" ::: a6, "c7" ::: a7]
  ToFields (a1, a2, a3, a4, a5, a6, a7, a8) = '["c1" ::: a1, "c2" ::: a2, "c3" ::: a3, "c4" ::: a4, "c5" ::: a5, "c6" ::: a6, "c7" ::: a7, "c8" ::: a8]

-- | zips a list of symbols with types
type ToFields'' :: [GL.Symbol] -> [Type] -> [(GL.Symbol, Type)]
type family ToFields'' fs as where
  ToFields'' _ '[] = '[]
  ToFields'' '[] (_ ': _) = GL.TypeError ( 'GL.Text "ToFields'': not enough symbols for the fields")
  ToFields'' (f ': fs) (a ': as) = '(f, a) ': ToFields'' fs as

-- | 'recLen' returns number of elements in PFoldable container
recLen :: forall rs. KnownNat (TP.Length rs) => Int -- let ghc figure it out (polykinds)
recLen = fromIntegral (natVal (Proxy @(TP.Length rs)))

-- | 'recLenP' same as 'recLen' but with an explicit proxy
recLenP :: forall rs p. KnownNat (TP.Length rs) => p rs -> Int
recLenP _ = recLen @rs -- fromIntegral (natVal (Proxy @(TP.Length rs)))

-- | 'recGet' gets the first value of a given type from a vinyl V.Identity list
recGet ::
  forall c record rs.
  ( RecElemFCtx record V.Identity
  , RecElem record c c rs rs (RIndex c rs)
  ) =>
  record V.Identity rs ->
  c
recGet = V.getIdentity . rget

-- todo: fix this: forces "k" so have add @_ for everything if I use a StandaloneKindSignature
-- could break in 9.2
-- type IndexType :: forall k. Nat -> [k] -> k -> Constraint

-- | class for indexing into a vinyl record
class IndexType (n :: Nat) rs a | n rs -> a where
  fromIx ::
    Rec f rs ->
    -- | get a value from a record
    f a
  setRec ::
    f a ->
    Rec f rs ->
    -- | set a value in a record
    Rec f rs

  fromIxP :: Proxy n -> Rec f rs -> f a
  fromIxP _ = fromIx @n

instance IndexType 'Z (r ': rs) r where
  fromIx (r :& _) = r
  setRec fa (_ :& rs) = fa :& rs

instance IndexType n rs a => IndexType ( 'S n) (r ': rs) a where
  fromIx (_ :& rs) = fromIx @n rs
  setRec fa (r :& rs) = r :& setRec @n fa rs

-- | 'rind' safely indexes into a vinyl list of V.Identity
rind :: forall n a rs. (IndexType (ToP n) rs a) => Rec V.Identity rs -> a
rind = V.getIdentity . fromIx @(ToP n)

-- | 'rind'' safely indexes into a vinyl list
rind' :: forall n f a rs. (IndexType (ToP n) rs a) => Rec f rs -> f a
rind' = fromIx @(ToP n)

-- | a lens into a vinyl record similar to ix but no maybe needed but works for Nat
rix :: forall n f a rs. IndexType (ToP n) rs a => Lens' (Rec f rs) (f a)
rix afa r = (\x -> setRec @(ToP n) @rs x r) <$> afa (fromIx @(ToP n) r)

-- | 'rixI' is the same as 'rix' but for 'V.Identity'
rixI :: forall n a rs. IndexType (ToP n) rs a => Lens' (Rec V.Identity rs) a
rixI afa r = (\x -> setRec @(ToP n) @rs (V.Identity x) r) <$> afa (V.getIdentity (fromIx @(ToP n) r))

-- | 'rixF' creates a lens for a named record
rixF :: forall n rs s a. IndexType (ToP n) rs '(s, a) => Lens' (Rec ElField rs) a
rixF = rix @n . elLens

-- todo: we can change the type of 'a' to 'b' but for Rec ElField rs we need to somehow replace a with b inside rs
-- but are only really utilising Lens' (ElField '(s,a)) a

-- | lens into the vinyl 'ElField' value
elLens :: Lens (ElField '(s, a)) (ElField '(s, b)) a b
elLens afb (Field a) = Field <$> afb a

-- can change the fieldname at the same time but have to be explicit
-- changed order so 't' is easier to get to

-- | lens into the vinyl 'ElField' label and value
elLens1 :: forall t s a b. GL.KnownSymbol t => Lens (ElField '(s, a)) (ElField '(t, b)) a b
elLens1 afb (Field a) = Field <$> afb a

-- | convenience patterns for creating and destructing a vinyl record of 'V.identity'
{-# COMPLETE I1 #-}

pattern I1 :: a -> Rec V.Identity '[a]
pattern I1 a = V.Identity a :& RNil

-- | convenience patterns for creating and destructing a vinyl record of 'V.identity'
{-# COMPLETE I2 #-}

pattern I2 :: a -> b -> Rec V.Identity '[a, b]
pattern I2 a b = V.Identity a :& V.Identity b :& RNil

-- | convenience patterns for creating and destructing a vinyl record of 'V.identity'
{-# COMPLETE I3 #-}

pattern I3 :: a -> b -> c -> Rec V.Identity '[a, b, c]
pattern I3 a b c = V.Identity a :& V.Identity b :& V.Identity c :& RNil

-- | convenience patterns for creating and destructing a vinyl record of 'V.identity'
{-# COMPLETE I4 #-}

pattern I4 :: a -> b -> c -> d -> Rec V.Identity '[a, b, c, d]
pattern I4 a b c d = V.Identity a :& V.Identity b :& V.Identity c :& V.Identity d :& RNil

-- | convenience patterns for creating and destructing a vinyl record of 'V.identity'
{-# COMPLETE I5 #-}

pattern I5 :: a -> b -> c -> d -> e -> Rec V.Identity '[a, b, c, d, e]
pattern I5 a b c d e = V.Identity a :& V.Identity b :& V.Identity c :& V.Identity d :& V.Identity e :& RNil

-- | convenience patterns for creating and destructing a vinyl record of 'V.identity'
{-# COMPLETE I6 #-}

pattern I6 :: a -> b -> c -> d -> e -> f -> Rec V.Identity '[a, b, c, d, e, f]
pattern I6 a b c d e f = V.Identity a :& V.Identity b :& V.Identity c :& V.Identity d :& V.Identity e :& V.Identity f :& RNil

-- | convenience patterns for creating and destructing a vinyl record of any type f
{-# COMPLETE E1 #-}

pattern E1 :: f a -> Rec f '[a]
pattern E1 fa = fa :& RNil

-- | convenience patterns for creating and destructing a vinyl record of any type f
{-# COMPLETE E2 #-}

pattern E2 :: f a -> f b -> Rec f '[a, b]
pattern E2 fa fb = fa :& fb :& RNil

-- | convenience patterns for creating and destructing a vinyl record of any type f
{-# COMPLETE E3 #-}

pattern E3 :: f a -> f b -> f c -> Rec f '[a, b, c]
pattern E3 fa fb fc = fa :& fb :& fc :& RNil

-- | convenience patterns for creating and destructing a vinyl record of any type f
{-# COMPLETE E4 #-}

pattern E4 :: f a -> f b -> f c -> f d -> Rec f '[a, b, c, d]
pattern E4 fa fb fc fd = fa :& fb :& fc :& fd :& RNil

-- | convenience patterns for creating and destructing a vinyl record of any type f
{-# COMPLETE E5 #-}

pattern E5 :: f a -> f b -> f c -> f d -> f e -> Rec f '[a, b, c, d, e]
pattern E5 fa fb fc fd fe = fa :& fb :& fc :& fd :& fe :& RNil

-- | convenience patterns for creating and destructing a vinyl record of any type f
{-# COMPLETE E6 #-}

pattern E6 :: f a -> f b -> f c -> f d -> f e -> f f' -> Rec f '[a, b, c, d, e, f']
pattern E6 fa fb fc fd fe ff = fa :& fb :& fc :& fd :& fe :& ff :& RNil

-- | create default values for types including named and unnamed vinyl records
type DefVal :: Type -> Constraint
class DefVal a where
  defVal :: a
  default defVal :: Monoid a => a
  defVal = mempty

instance DefVal Char where
  defVal = '\0'
instance DefVal Bool where
  defVal = False
instance DefVal Int where
  defVal = 0
instance DefVal Natural where
  defVal = 0
instance DefVal Integer where
  defVal = 0
instance DefVal Double where
  defVal = 0
instance DefVal Float where
  defVal = 0
instance DefVal Rational where
  defVal = 0

instance DefVal ()
instance DefVal [a]
instance DefVal T.Text
instance DefVal BS.ByteString
instance DefVal BL.ByteString

instance DefVal (M.Map k v) where
  defVal = M.empty
instance DefVal (IM.IntMap v)
instance DefVal (Set.Set v) where
  defVal = Set.empty
instance DefVal (Maybe a) where
  defVal = Nothing
instance DefVal e => DefVal (Either e a) where
  defVal = Left defVal
instance DefVal e => DefVal (These e a) where
  defVal = This defVal
instance (DefVal a, DefVal b) => DefVal (a, b) where
  defVal = (defVal, defVal)
instance (DefVal a, DefVal b, DefVal c) => DefVal (a, b, c) where
  defVal = (defVal, defVal, defVal)
instance (DefVal a, DefVal b, DefVal c, DefVal d) => DefVal (a, b, c, d) where
  defVal = (defVal, defVal, defVal, defVal)
instance DefVal a => DefVal (V.Identity a) where
  defVal = V.Identity defVal
instance (GL.KnownSymbol s, DefVal t) => DefVal (ElField '(s, t)) where
  defVal = Field defVal
instance DefVal (Rec f '[])
instance (DefVal (Rec f rs), DefVal (f r)) => DefVal (Rec f (r ': rs)) where
  defVal = defVal :& defVal
