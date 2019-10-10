-- consider moving RemoveType and RemoveField typefamilies to typelevel/PCore
-- could use generics for DefVal
-- cant promote strings to typelevel
{- make sure ToFields' is fast else you broke it cos not lazy enough
>:kind! ToFields' '[Int,Double]
ToFields' '[Int,Double] :: [(Symbol, *)]
= '['("c1", Int), '("c2", Double)]

>:kind! ToFields' P.$$ P.Mconcat P.$$ P.Replicate 10 '[Int,Double]
ToFields' P.$$ P.Mconcat P.$$ P.Replicate 10 '[Int,Double] :: [(Symbol,
                                                                *)]
= '['("c1", Int), '("c2", Double), '("c3", Int), '("c4", Double),
    '("c5", Int), '("c6", Double), '("c7", Int), '("c8", Double),
    '("c9", Int), '("c10", Double), '("c11", Int), '("c12", Double),
    '("c13", Int), '("c14", Double), '("c15", Int), '("c16", Double),
    '("c17", Int), '("c18", Double), '("c19", Int), '("c20", Double)]
-}
{-# OPTIONS -Wall -Wcompat -Wincomplete-record-updates -Wincomplete-uni-patterns #-}
-- {-# OPTIONS -Wno-redundant-constraints #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeInType #-} -- data ToFieldsImplSym1 :: ...
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-} -- need this for DEF show instance and Writeable stuff cos calls another type family ie Or
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE AllowAmbiguousTypes #-} -- need this cos of fromIx
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE DefaultSignatures #-}
{- |
Module      : VinylUtils
Description : utilities for holds a singleton value 'One'
Copyright   : (c) Grant Weyburne, 2016
License     : GPL-3
Maintainer  : gbwey9@gmail.com

handles a tuple of size one
-}
module VinylUtils where
import Data.Vinyl
import Data.Vinyl.TypeLevel
import qualified Data.Vinyl.Functor as V
import Data.Proxy
import GHC.TypeLits (ErrorMessage((:<>:)),Symbol,KnownSymbol)
import qualified GHC.TypeLits as GL -- (GL.GL.GL.TypeError,ErrorMessage(..))
import GHC.TypeNats
import GHC.Natural
import Data.Type.Equality
import One
import qualified PCombinators as P
import PCombinators (type (@@), type (:..:), type (:.:), type (<*>), type (<$>), type (<>), SAppSym0, SAppSym1, Length, type (~>), Apply)
import Data.Kind (Type)
import Control.Lens (type Lens', type Lens)
import qualified Frames as F
import Frames ((:->))
import qualified Control.Foldl as FL
import qualified Control.Scanl as FS
import qualified Control.Monad.State.Strict as S
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.IntMap as IM
import qualified Data.Set as Set
import Data.These

type F = Rec ElField -- :: [(Symbol, Type)] -> Type
type WW = Rec V.Identity -- :: [Type] -> Type

-- if you cant be bothered naming the fields manually then this will do it for you
-- useful if you want to convert to a Frame and run stats on the frame
-- | lift a ntuple of 'Type's to a promoted list of named fields for use with Rec ElField
type family MakeF (a :: Type) :: Type where
  MakeF a = Rec ElField (ToFields a)

-- | lift a promoted list of unnamed fields to named fields for use with Rec ElField
type family MakeF' a where
  MakeF' a = Rec ElField (ToFields' a)

-- | 'ToFields' assigns fieldnames to unnamed fields for tuples up to size 8
type family ToFields (a :: Type) = (ret :: [(Symbol,Type)]) | ret -> a where
  ToFields (One a)                   = '["c1" ::: a]
  ToFields (a1,a2)                   = '["c1" ::: a1, "c2" ::: a2]
  ToFields (a1,a2,a3)                = '["c1" ::: a1, "c2" ::: a2, "c3" ::: a3]
  ToFields (a1,a2,a3,a4)             = '["c1" ::: a1, "c2" ::: a2, "c3" ::: a3, "c4" ::: a4]
  ToFields (a1,a2,a3,a4,a5)          = '["c1" ::: a1, "c2" ::: a2, "c3" ::: a3, "c4" ::: a4, "c5" ::: a5]
  ToFields (a1,a2,a3,a4,a5,a6)       = '["c1" ::: a1, "c2" ::: a2, "c3" ::: a3, "c4" ::: a4, "c5" ::: a5, "c6" ::: a6]
  ToFields (a1,a2,a3,a4,a5,a6,a7)    = '["c1" ::: a1, "c2" ::: a2, "c3" ::: a3, "c4" ::: a4, "c5" ::: a5, "c6" ::: a6, "c7" ::: a7]
  ToFields (a1,a2,a3,a4,a5,a6,a7,a8) = '["c1" ::: a1, "c2" ::: a2, "c3" ::: a3, "c4" ::: a4, "c5" ::: a5, "c6" ::: a6, "c7" ::: a7, "c8" ::: a8]
  --ToFields a                         = GL.TypeError ('GL.Text "only caters for (One a) and tuples up to size 8!")

--  ToFields' as = ToFieldsImpl (XSS X999) as  -- X9 for small X99 for medium and X999 for very large

-- | 'ToFields'' assigns fieldnames to a promoted list of unnamed fields starting with c1 c2 ... c999
type family ToFields' (as :: [Type]) :: [(Symbol, Type)] where
  ToFields' as = P.CaseAll
    '[ '(P.BetweenSym2 0 9, ToFields'ImplSym2 'SMALL as)
     , '(P.BetweenSym2 10 99, ToFields'ImplSym2 'MEDIUM as)
     , '(P.BetweenSym2 100 999, ToFields'ImplSym2 'LARGE  as)
     ] (GL.TypeError ('GL.Text "ToFields' too many fields length=" ':<>: 'GL.ShowType (Length as)))
     (Length as)

data ToFields'ImplSym0 :: SZ ~> [Type] ~> Bool ~> [(Symbol,Type)]
type instance Apply ToFields'ImplSym0 x = ToFields'ImplSym1 x

data ToFields'ImplSym1 :: SZ -> [Type] ~> Bool ~> [(Symbol,Type)]
type instance Apply (ToFields'ImplSym1 x) y = ToFields'ImplSym2 x y

data ToFields'ImplSym2 :: SZ -> [Type] -> Bool ~> [(Symbol,Type)]
type instance Apply (ToFields'ImplSym2 x y) z = ToFields'Impl x y z

-- have to push all strict stuff down here: X999 is a live grenade!
-- | 'ToFields'Impl' careful! this is here because type level stuff is strict and we dont want to pay the cost of unnecessary long compile times!
type family ToFields'Impl (sz :: SZ) (as :: [Type]) (b :: Bool) :: [(Symbol, Type)] where
  ToFields'Impl 'SMALL as 'True = ToFieldsImpl (XSS X9) as
  ToFields'Impl 'MEDIUM as 'True = ToFieldsImpl (XSS X99) as
  ToFields'Impl 'LARGE as 'True = ToFieldsImpl (XSS X999) as
  ToFields'Impl x as 'False = GL.TypeError ('GL.Text "ToFields'Impl: shouldnt be called")

data SZ = SMALL | MEDIUM | LARGE

type family XSS (fs :: [Symbol]) :: [Symbol] where
  XSS fs = SAppSym1 "c" <$> fs

data XSSSym0 :: [Symbol] ~> [Symbol]
type instance Apply XSSSym0 x = XSS x

-- | 'X9' handles stringified numbers from 1 to 9
type family X9 where
  X9  = '["1","2","3","4","5","6","7","8","9"]

-- | 'X99' handles stringified numbers from 1 to 99
type family X99 where
  X99 = X9 <> (SAppSym0 <$> X9 <*> ("0" ': X9))

-- | 'X999' handles stringified numbers from 1 to 999
type family X999 where
  X999 = X99 <> ((SAppSym0 :..: SAppSym0) <$> X9 <*> ("0" ': X9) <*> ("0" ': X9))

-- we want it to fail if the lengths dont match: this ensures that we must have the right number of fields
type family ToFieldsImpl (fs :: [Symbol]) (as :: [Type]) :: [(Symbol, Type)] where
  ToFieldsImpl fs as = P.ZipWithExact P.PairSym0 (P.Take (P.Length as) fs) as

data ToFieldsImplSym1 :: [Symbol] -> [Type] ~> [(Symbol, Type)]
type instance Apply (ToFieldsImplSym1 x) y = ToFieldsImpl x y

-- | 'recLen' returns number of elements in PFoldable container
recLen :: forall rs . KnownNat (P.Length rs) => Int    -- let ghc figure it out (polykinds)
recLen = fromIntegral (natVal (Proxy @(P.Length rs)))

-- | 'recLenP' same as 'recLen' but with an explicit proxy
recLenP :: forall rs p . KnownNat (P.Length rs) => p rs -> Int
recLenP _ = recLen @rs -- fromIntegral (natVal (Proxy @(P.Length rs)))

-- | 'recGet' gets the first value of a given type from a vinyl V.Identity list
recGet :: forall c record rs . (RecElemFCtx record V.Identity,
      RecElem record c c rs rs (RIndex c rs)) =>
     record V.Identity rs -> c
recGet = V.getIdentity . rget

class IndexType (n :: P.N) (rs :: [k]) a | n rs -> a where
  fromIx :: Rec f rs -> f a
  setRec :: f a -> Rec f rs -> Rec f rs

  fromIxP :: Proxy n -> Rec f rs -> f a
  fromIxP _ = fromIx @n

--  rixN :: Lens' (Rec f rs) (f a)
--  rixN afa r = (\w -> setRec @n @rs w r) <$> afa (fromIx @n r)


instance IndexType 'P.Z (r ': rs) r where
  fromIx (r :& _) = r
  setRec fa (_ :& rs) = fa :& rs

instance IndexType n rs a => IndexType ('P.S n) (r ': rs) a where
  fromIx (_ :& rs) = fromIx @n rs
  setRec fa (r :& rs) = r :& setRec @n fa rs

-- | 'rind' safely indexes into a vinyl list of V.Identity
rind :: forall n a rs. (IndexType (P.ToN n) rs a) => Rec V.Identity rs -> a
rind = V.getIdentity . fromIx @(P.ToN n)

-- | 'rind'' safely indexes into a vinyl list
rind' :: forall n f a rs. (IndexType (P.ToN n) rs a) => Rec f rs -> f a
rind' = fromIx @(P.ToN n)

-- | a lens into a vinyl record similar to ix but no maybe needed but works for Nat
rix :: forall n f a rs. IndexType (P.ToN n) rs a => Lens' (Rec f rs) (f a)
rix afa r = (\x -> setRec @(P.ToN n) @rs x r) <$> afa (fromIx @(P.ToN n) r)

-- | 'rixI' is the same as 'rix' but for 'V.Identity'
rixI :: forall n a rs. IndexType (P.ToN n) rs a => Lens' (Rec V.Identity rs) a
rixI afa r = (\x -> setRec @(P.ToN n) @rs (V.Identity x) r) <$> afa (V.getIdentity (fromIx @(P.ToN n) r))

-- | 'rixF' creates a lens for a named record
rixF :: forall n rs s a . IndexType (P.ToN n) rs '(s, a) => Lens' (Rec ElField rs) a
rixF = rix @n . elLens

-- todo: we can change the type of 'a' to 'b' but for Rec ElField rs we need to somehow replace a with b inside rs
-- but are only really utilising Lens' (ElField '(s,a)) a
elLens :: Lens (ElField '(s,a)) (ElField '(s,b)) a b
elLens afb (Field a) = Field <$> afb a

-- can change the fieldname at the same time but have to be explicit
-- changed order so 't' is easier to get to
elLens1 :: forall t s a b . KnownSymbol t => Lens (ElField '(s,a)) (ElField '(t,b)) a b
elLens1 afb (Field a) = Field <$> afb a

-- | convenience patterns for creating and destructing a vinyl record of 'V.identity'
{-# COMPLETE I1 #-}
pattern I1 :: a -> Rec V.Identity '[a]
pattern I1 a = V.Identity a :& RNil

{-# COMPLETE I2 #-}
pattern I2 :: a -> b -> Rec V.Identity '[a,b]
pattern I2 a b = V.Identity a :& V.Identity b :& RNil

{-# COMPLETE I3 #-}
pattern I3 :: a -> b -> c -> Rec V.Identity '[a,b,c]
pattern I3 a b c = V.Identity a :& V.Identity b :& V.Identity c :& RNil

{-# COMPLETE I4 #-}
pattern I4 :: a -> b -> c -> d -> Rec V.Identity '[a,b,c,d]
pattern I4 a b c d = V.Identity a :& V.Identity b :& V.Identity c :& V.Identity d :& RNil

{-# COMPLETE I5 #-}
pattern I5 :: a -> b -> c -> d -> e -> Rec V.Identity '[a,b,c,d,e]
pattern I5 a b c d e = V.Identity a :& V.Identity b :& V.Identity c :& V.Identity d :& V.Identity e :& RNil

{-# COMPLETE I6 #-}
pattern I6 :: a -> b -> c -> d -> e -> f -> Rec V.Identity '[a,b,c,d,e,f]
pattern I6 a b c d e f = V.Identity a :& V.Identity b :& V.Identity c :& V.Identity d :& V.Identity e :& V.Identity f :& RNil

-- | convenience patterns for creating and destructing a vinyl record of any type f
{-# COMPLETE E1 #-}
pattern E1 :: f a -> Rec f '[a]
pattern E1 fa = fa :& RNil

{-# COMPLETE E2 #-}
pattern E2 :: f a -> f b -> Rec f '[a,b]
pattern E2 fa fb = fa :& fb :& RNil

{-# COMPLETE E3 #-}
pattern E3 :: f a -> f b -> f c -> Rec f '[a,b,c]
pattern E3 fa fb fc = fa :& fb :& fc :& RNil

{-# COMPLETE E4 #-}
pattern E4 :: f a -> f b -> f c -> f d -> Rec f '[a,b,c,d]
pattern E4 fa fb fc fd = fa :& fb :& fc :& fd :& RNil

{-# COMPLETE E5 #-}
pattern E5 :: f a -> f b -> f c -> f d -> f e -> Rec f '[a,b,c,d,e]
pattern E5 fa fb fc fd fe = fa :& fb :& fc :& fd :& fe :& RNil

{-# COMPLETE E6 #-}
pattern E6 :: f a -> f b -> f c -> f d -> f e -> f f' -> Rec f '[a,b,c,d,e,f']
pattern E6 fa fb fc fd fe ff = fa :& fb :& fc :& fd :& fe :& ff :& RNil

-- works for any Foldable (F rs) so works for Frame or []
frameToList :: forall rs t . (StripFieldNames rs, Foldable t) => t (F rs) -> [Rec V.Identity (Unlabeled rs)]
frameToList = foldMap ((:[]) . F.stripNames)

-- | create a scan using a fold for a vinyl record and stash it in a new field called s1: eg create a column with a running average
postscanF :: forall s1 b us . (KnownSymbol s1)
  => FL.Fold (Rec V.ElField us) b -> FS.Scan (Rec V.ElField us) (Rec V.ElField ((s1 :-> b) ': us))
postscanF (FL.Fold step begin done) = FS.Scan (S.state . step') begin
  where
    step' a x = (V.Field @s1 b :& a, x')
      where
        x' = step x a
        b  = done x'

prescanF :: forall s1 b us . (KnownSymbol s1)
  => FL.Fold (Rec V.ElField us) b -> FS.Scan (Rec V.ElField us) (Rec V.ElField ((s1 :-> b) ': us))
prescanF (FL.Fold step begin done) = FS.Scan (S.state . step') begin
  where
    step' a x = (V.Field @s1 b :& a, x')
      where
        x' = step x a
        b  = done x

-- | changes the field names of an Rec ElField rs if there are duplicates
uniqueRec :: forall rs . UniqueRecImpl rs (P.Foldl P.UniquePairImplSym0 '[] rs)
         => Rec ElField rs -> Rec ElField (P.UniquePair rs)
uniqueRec = uniqueRecImpl @_ @(P.UniquePair rs)

class UniqueRecImpl rs rs1 where
  uniqueRecImpl :: Rec ElField rs -> Rec ElField rs1

instance UniqueRecImpl '[] '[] where
  uniqueRecImpl RNil = RNil

instance (KnownSymbol s1, UniqueRecImpl rs rs1) => UniqueRecImpl ('(s,t) ': rs) ('(s1,t) ': rs1) where
  uniqueRecImpl (Field r :& rs) = Field @s1 r :& uniqueRecImpl @rs @rs1 rs

-- | create default values for types including named and unnamed vinyl records
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
instance (DefVal a, DefVal b) => DefVal (a,b) where
  defVal = (defVal, defVal)
instance (DefVal a, DefVal b, DefVal c) => DefVal (a,b,c) where
  defVal = (defVal, defVal, defVal)
instance (DefVal a, DefVal b, DefVal c, DefVal d) => DefVal (a,b,c,d) where
  defVal = (defVal, defVal, defVal, defVal)
instance DefVal a => DefVal (V.Identity a) where
  defVal = V.Identity defVal
instance (KnownSymbol s, DefVal t) => DefVal (ElField '(s,t)) where
  defVal = Field defVal
instance DefVal (Rec f '[])
instance (DefVal (Rec f rs), DefVal (f r)) => DefVal (Rec f (r ': rs)) where
  defVal = defVal :& defVal

-- | removes first occurence of named field
-- bearbeiten: has to be a better way -- tricky getting stuff to value level from type level
type family RemoveField (arg :: Symbol) (rs :: [(Symbol,Type)]) :: [(Symbol,Type)] where
  RemoveField s '[] = '[] -- should be a typeerror?
  RemoveField s ('(s,t) ': rs) = rs -- forgot about pattern match by name: ie s == s
  RemoveField s ('(s1,t) ': rs) = '(s1,t) ': RemoveField s rs

removeField :: forall s rs . (RemoveFieldImpl (MatchField s rs) s rs) => Rec ElField rs -> Rec ElField (RemoveField s rs)
removeField rec = removeFieldImpl @(MatchField s rs) @s rec

class RemoveFieldImpl b s rs where
  removeFieldImpl :: Rec ElField rs -> Rec ElField (RemoveField s rs)
instance RemoveFieldImpl b s '[] where
  removeFieldImpl RNil = RNil
-- need to distinguish these instances to avoid overlap so using kind Bool which is very awkward
instance (RemoveField s ('(s, t) : rs) ~ rs) => RemoveFieldImpl 'True s ('(s,t) ': rs) where
  removeFieldImpl (Field _ :& rs) = rs
instance (KnownSymbol s1, RemoveFieldImpl (MatchField s rs) s rs, RemoveField s ('(s1, t) : rs) ~ ('(s1, t) : RemoveField s rs)) => RemoveFieldImpl 'False s ('(s1,t) ': rs) where
  removeFieldImpl (Field t :& rs) = Field @s1 t :& removeFieldImpl @(MatchField s rs) @s @rs rs

type family MatchField (arg :: Symbol) (rs :: [(Symbol,k)]) :: Bool where
  MatchField s '[] = 'False
  MatchField s ('(s1,t) ': rs) = s == s1 -- is there a difference between pattern match and ==?

type family RemoveType (arg :: Type) (rs :: [(Symbol,k)]) :: [(Symbol,k)] where
  RemoveType t '[] = '[]
  RemoveType t ('(s,t) ': rs) = rs -- forgot about pattern match by name: ie s == s works so dont need If
  RemoveType t ('(s,t1) ': rs) = '(s,t1) ': RemoveType t rs

removeType :: forall t rs . (RemoveTypeImpl (MatchType t rs) t rs) => Rec ElField rs -> Rec ElField (RemoveType t rs)
removeType rec = removeTypeImpl @(MatchType t rs) @t rec

class RemoveTypeImpl b t rs where
  removeTypeImpl :: Rec ElField rs -> Rec ElField (RemoveType t rs)
instance RemoveTypeImpl b t '[] where
  removeTypeImpl RNil = RNil
-- need to distinguish these instances (overlap) using kind Bool which is awkward
instance (RemoveType t ('(s, t) : rs) ~ rs) => RemoveTypeImpl 'True t ('(s,t) ': rs) where
  removeTypeImpl (Field _ :& rs) = rs
instance (RemoveTypeImpl (MatchType t rs) t rs, RemoveType t ('(s, t1) : rs) ~ ('(s, t1) : RemoveType t rs)) => RemoveTypeImpl 'False t ('(s,t1) ': rs) where
  removeTypeImpl (Field t :& rs) = Field @s t :& removeTypeImpl @(MatchType t rs) @t @rs rs

type family MatchType (arg :: Type) (rs :: [(Symbol,k)]) :: Bool where
  MatchType t '[] = 'False
  MatchType t ('(s,t) ': rs) = 'True
  MatchType t ('(s,t1) ': rs) = 'False

{- doesnt work with polymorphic types unless there is a match just before that field and doesnt need to go further
-- only for removeType but removeField has no problem cos all labels!
>removeType @(Maybe Int) (#aa =: True :& #bb =: (123::Double) :& #cc =: 'x' :& #aa =: ("asdf"::String) :& RNil)
{aa :-> True, bb :-> 123.0, cc :-> 'x', aa :-> "asdf"}
it ::
  Rec
    ElField
    '['("aa", Bool), '("bb", Double), '("cc", Char), '("aa", [Char])]
-}



{-
>removeField @"aa" (#aa1 =: True :& #bb =: 123 :& RNil)
{aa1 :-> True, bb :-> 123}
it :: Num v => Rec ElField '['("aa1", Bool), '("bb", v)]

>removeField @"aa" (#aa =: True :& #bb =: 123 :& #cc =: 'x' :& RNil)
{bb :-> 123, cc :-> 'x'}
it :: Num v => Rec ElField '["bb" ::: v, "cc" ::: Char]

>removeField @"bb" (#aa =: True :& #bb =: 123 :& #cc =: 'x' :& RNil)
{aa :-> True, cc :-> 'x'}
it :: Rec ElField '['("aa", Bool), "cc" ::: Char]

>removeField @"cc" (#aa =: True :& #bb =: 123 :& #cc =: 'x' :& RNil)
{aa :-> True, bb :-> 123}
it :: Num v => Rec ElField '['("aa", Bool), '("bb", v)]

>removeFieldImpl @'True @"aa" (#aa =: True :& #bb =: 123 :& RNil)
{bb :-> 123}
it :: Num v => Rec ElField '["bb" ::: v]

>removeFieldImpl @'False @"aa" (#aa1 =: True :& #bb =: 123 :& RNil)
{aa1 :-> True, bb :-> 123}
it :: Num v => Rec ElField '['("aa1", Bool), '("bb", v)]

>removeFieldImpl @'False @"aa" (#aa1 =: True :& #aa =: 123 :& RNil)
{aa1 :-> True}
it :: Rec ElField '['("aa1", Bool)]

>removeField @"aa" (#aa =: True :& #bb =: 123 :& #cc =: 'x' :& #aa =: "asdf" :& RNil)
{bb :-> 123, cc :-> 'x', aa :-> "asdf"}
it ::
  (Num v1, IsString v2) =>
  Rec ElField '["bb" ::: v1, "cc" ::: Char, "aa" ::: v2]

>removeField @"aa" $ removeField @"aa" (#aa =: True :& #bb =: 123 :& #cc =: 'x' :& #aa =: "asdf" :& RNil)
{bb :-> 123, cc :-> 'x'}
it :: Num v => Rec ElField '['("bb", v), '("cc", Char)]
-}
{-
type family Test1 (s :: Symbol) (s1 :: Symbol) where
  Test1 s s = 'True
  Test1 s s1 = 'False
-}
type family RemoveTypes (arg :: Type) (rs :: [(Symbol,k)]) :: [(Symbol,k)] where
  RemoveTypes t '[] = '[]
  RemoveTypes t ('(s,t) ': rs) = RemoveTypes t rs
  RemoveTypes t ('(s,t1) ': rs) = '(s,t1) ': RemoveTypes t rs

removeTypes :: forall t rs . (RemoveTypesImpl (MatchType t rs) t rs) => Rec ElField rs -> Rec ElField (RemoveTypes t rs)
removeTypes rec = removeTypesImpl @(MatchType t rs) @t rec

class RemoveTypesImpl b t rs where
  removeTypesImpl :: Rec ElField rs -> Rec ElField (RemoveTypes t rs)
instance RemoveTypesImpl b t '[] where
  removeTypesImpl RNil = RNil
-- need to distinguish these instances (overlap) using kind Bool which is awkward
instance (RemoveTypesImpl (MatchType t rs) t rs, RemoveTypes t ('(s, t) : rs) ~ RemoveTypes t rs) => RemoveTypesImpl 'True t ('(s,t) ': rs) where
  removeTypesImpl (Field _ :& rs) = removeTypesImpl @(MatchType t rs) @t @rs rs
instance (RemoveTypesImpl (MatchType t rs) t rs, RemoveTypes t ('(s, t1) : rs) ~ ('(s, t1) : RemoveTypes t rs)) => RemoveTypesImpl 'False t ('(s,t1) ': rs) where
  removeTypesImpl (Field t :& rs) = Field @s t :& removeTypesImpl @(MatchType t rs) @t @rs rs


-- yurk : need a way to generalise this
type family RemoveOn (p :: k ~> Bool) (rs :: [k]) :: [k] where
  RemoveOn p '[] = '[]
  RemoveOn p (r ': rs) = P.If (p @@ r) (RemoveOn p rs) (r ': RemoveOn p rs)

class RemoveOnImpl (b :: Bool) (s :: Symbol) (rs :: [(Symbol,Type)]) where
  removeOnImpl :: Rec ElField rs -> Rec ElField (RemoveOn (P.EqSym1 s :.: P.FstSym0) rs)
instance RemoveOnImpl b s '[] where
  removeOnImpl RNil = RNil
instance (RemoveOnImpl (MatchField s rs) s rs, RemoveOn (P.EqSym1 s P.:.: P.FstSym0) ('(s, t) : rs) ~ RemoveOn (P.EqSym1 s P.:.: P.FstSym0) rs) => RemoveOnImpl 'True s ('(s,t) ': rs) where
  removeOnImpl (_ :& rs) = removeOnImpl @(MatchField s rs) @s @rs rs
instance (RemoveOnImpl (MatchField s rs) s rs, P.If (s == s1) (RemoveOn (P.EqSym1 s :.: P.FstSym0) rs) ('(s1, t) : RemoveOn (P.EqSym1 s :.: P.FstSym0) rs) ~ ('(s1, t) : RemoveOn (P.EqSym1 s :.: P.FstSym0) rs)) => RemoveOnImpl 'False s ('(s1,t) ': rs) where
  removeOnImpl (z :& rs) = z :& removeOnImpl @(MatchField s rs) @s @rs rs

removeOn :: forall s rs . RemoveOnImpl (MatchField s rs) s rs => Rec ElField rs -> Rec ElField (RemoveOn (P.EqSym1 s :.: P.FstSym0) rs)
removeOn rec = removeOnImpl @(MatchField s rs) @s @rs rec

{-
>removeOnImpl @'True @"aa" (#aa =: True :& #bb =: (123::Double) :& #cc =: 'x' :& #aa1 =: "asdf"  :& RNil)
{bb :-> 123.0, cc :-> 'x', aa1 :-> "asdf"}
it ::
  Rec ElField '['("bb", Double), '("cc", Char), '("aa1", [Char])]
>removeOnImpl @'False @"b" (#aa =: True :& #bb =: (123::Double) :& #cc =: 'x' :& #aa1 =: "asdf"  :& RNil)
{aa :-> True, bb :-> 123.0, cc :-> 'x', aa1 :-> "asdf"}
it ::
  Rec
    ElField
    '['("aa", Bool), '("bb", Double), '("cc", Char), '("aa1", [Char])]

>removeOn  @"bb" (#aa =: True :& #bb =: (123::Double) :& #cc =: 'x' :& #aa =: "asdf"  :& RNil)
{aa :-> True, cc :-> 'x', aa :-> "asdf"}
it :: Rec ElField '['("aa", Bool), '("cc", Char), '("aa", [Char])]
>removeOn  @"aa" (#aa =: True :& #bb =: (123::Double) :& #cc =: 'x' :& #aa =: "asdf"  :& RNil)
{bb :-> 123.0, cc :-> 'x'}
it :: Rec ElField '['("bb", Double), '("cc", Char)]
-}

{-
type family MatchOn (s :: Symbol) (rs :: [(Symbol,k)]) :: Bool where
  MatchOn s '[] = 'False
  MatchOn s ('(s,t) ': rs) = 'True
  MatchOn s ('(s1,t) ': rs) = 'False
-}


{-
class RemoveOn1Impl (b :: Bool) (s :: Symbol) (rs :: [(Symbol,Type)]) where
  type Znork b s rs :: Bool
  type Znork b s rs = MatchField s rs

  removeOn1Impl :: Rec ElField rs -> Rec ElField (RemoveOn (P.EqSym1 s :.: P.FstSym0) rs)

instance RemoveOn1Impl (Znork b s '[]) s '[] where
  removeOn1Impl RNil = RNil
-}

{-
instance (RemoveOn1Impl (MatchField s rs) s rs, RemoveOn1 (P.EqSym1 s P.:.: P.FstSym0) ('(s, t) : rs) ~ RemoveOn1 (P.EqSym1 s P.:.: P.FstSym0) rs) => RemoveOn1Impl 'True s ('(s,t) ': rs) where
  removeOn1Impl (_ :& rs) = removeOn1Impl @(MatchField s rs) @s @rs rs
instance (RemoveOn1Impl (MatchField s rs) s rs, P.If (s == s1) (RemoveOn1 (P.EqSym1 s :.: P.FstSym0) rs) ('(s1, t) : RemoveOn1 (P.EqSym1 s :.: P.FstSym0) rs) ~ ('(s1, t) : RemoveOn1 (P.EqSym1 s :.: P.FstSym0) rs)) => RemoveOn1Impl 'False s ('(s1,t) ': rs) where
  removeOn1Impl (z :& rs) = z :& removeOn1Impl @(MatchField s rs) @s @rs rs
-}
