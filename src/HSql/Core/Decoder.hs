{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : HSql.Core.Decoder
Description : decode from 'SqlValue's to haskell values
Copyright   : (c) Grant Weyburne, 2016
License     : BSD-3

'Dec' defines a decoder and 'DefDec' has the default decoder for a given type.
-}
module HSql.Core.Decoder where

import Control.Applicative
import Control.Arrow (left)
import Control.DeepSeq (NFData)
import Control.Lens
import Control.Monad (replicateM)
import Control.Monad.State.Strict (StateT (..))
import Data.ByteString (ByteString)
import qualified Data.Functor.Identity as L
import Data.Kind
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as N
import Data.Proxy
import Data.Text (Text)
import Data.These
import Data.Time
import Data.Vinyl
import Data.Vinyl.CoRec (CoRec (..))
import qualified Data.Vinyl.CoRec as VC
import qualified Data.Vinyl.Functor as V
import Database.HDBC (SqlValue (..))
import DocUtils.Doc
import GHC.Generics
import GHC.Stack (HasCallStack)
import GHC.TypeLits (ErrorMessage (..), KnownNat, KnownSymbol, Nat, Symbol, natVal, symbolVal)
import qualified GHC.TypeLits as GL
import HSql.Core.Conv (Conv, conv)
import HSql.Core.ErrorHandler (ConvE (cvMessage), DE, DE', DecodingE (..), failDE, liftCE)
import HSql.Core.Raw
import Utils.Error
import Utils.Fold
import Utils.One
import qualified Utils.TypeLevel as TP (FailUnless, Snds)

-- | stateful parser for decoding sql values
newtype Dec a = Dec {unDec :: [SqlValue] -> Either DE (a, [SqlValue])}
  deriving stock (Generic, Functor)

instance NFData a => NFData (Dec a)

-- | prism to change a decoder
decPrism :: Show s => Prism' s a -> Dec s -> Dec a
decPrism p (Dec f) =
  Dec $ \xs -> do
    (s, xs') <- f xs
    case s ^? p of
      Nothing ->
        failDE
          "decPrism"
          ("prism failed s=" ++ psiS s)
          ""
          xs'
      Just a -> Right (a, xs')

-- | getter for decoder
decLens :: Getting a s a -> Dec s -> Dec a
decLens ll (Dec f) =
  Dec $ \xs -> f xs & _Right . _1 %~ (^. ll)

-- | lifts a decoder into a decoder of 'DM'
liftDM :: Dec a -> Dec (DM a)
liftDM = (DM . pure <$>)

-- | lifts a list of decoders into a decoder of 'DM'
liftDMs :: [Dec a] -> Dec (DM a)
liftDMs ds = DM <$> sequenceA ds

-- | 'DM' holds a list of decoders of the same type
newtype DM a = DM {unDM :: [a]}
  deriving stock (Show, Eq, Generic, Functor)
  deriving newtype (Semigroup, Monoid)

instance Semigroup a => Semigroup (Dec a) where
  f <> g = (<>) <$> f <*> g

instance Monoid a => Monoid (Dec a) where
  mempty = Dec $ \xs -> Right (mempty, xs)

instance Show (Dec a) where
  show Dec{} = "Dec<fn>"

-- | 'DecNE' holds a nonempty list of "n" decoders
newtype DecNE (n :: Nat) a = DecNE {unDecNE :: NonEmpty a}
  deriving stock (Show, Eq, Generic, Functor)

-- | 'DecN' holds a list of n decoders -- typelevel version of 'DM'
newtype DecN (n :: Nat) a = DecN {unDecN :: [a]}
  deriving stock (Show, Eq, Generic, Functor)

-- | 'DecAlle' holds a list of decoders but expects to decode all the values
newtype DecAlle a = DecAlle {unDecAlle :: [a]}
  deriving stock (Show, Eq, Generic, Functor)

instance Applicative Dec where
  pure a = Dec $ \xs -> Right (a, xs)
  Dec fab <*> Dec fa = Dec $ \rs -> do
    (ab, rs') <- fab rs
    (a, rs'') <- fa rs'
    return (ab a, rs'')

instance Alternative Dec where
  empty = Dec $ \_ -> failDE "Alternative" "" "" []
  Dec fa <|> Dec fb = Dec $ \rs -> case fa rs of
    Left _ -> fb rs
    Right ret -> Right ret

-- | decoder for any 'SqlValue'
decAny :: Dec SqlValue
decAny = decPred (const True)

-- | 'decPred' is a simple predicate on a raw value
decPred :: (SqlValue -> Bool) -> Dec SqlValue
decPred p = Dec $ \case
  x : xs
    | p x -> Right (x, xs)
    | otherwise -> failDE "decPred" "pred failed" (psiS (x : xs)) [x]
  [] -> failDE "decPred" "no data" "" []

-- | adds error context to a decoder
decAddError :: String -> String -> String -> Dec a -> Dec a
decAddError s t u (Dec d) =
  Dec $ \xs -> left' (decAddError' s t u xs) (d xs)

-- | adds error context to a decoder including 'SqlValue's
decAddError' :: String -> String -> String -> [SqlValue] -> DE -> DE
decAddError' s t u xs = (CoRec (V.Identity (DecodingE s t u xs)) N.<|)

-- | decoder for 'SqlNull'
decSqlNull :: Dec SqlValue
decSqlNull = decPred (== SqlNull)

-- | decoder for 'Maybe'
decMaybe :: Dec a -> Dec (Maybe a)
decMaybe d = decAddError "decMaybe" "" "" $ Nothing <$ decPred (== SqlNull) <|> Just <$> d

instance Monad Dec where
  return = pure
  Dec fa >>= amb = Dec $ \rs -> do
    (a, rs') <- fa rs
    unDec (amb a) rs'

-- | convert a single decoder to work on a list
decAlle :: forall a. Dec a -> Dec (DecAlle a)
decAlle (Dec d) =
  Dec $ \hhs -> left' (decAddError' "Dec DecAlle" "" "" hhs) $ do
    as <- unfoldM (\hhs' -> if null hhs' then return Nothing else Just <$> d hhs') hhs
    return (DecAlle as, [])

-- | generic decoder
decGeneric :: Conv a => Dec a
decGeneric = Dec $ \case
  (c : cs) -> left' (decAddError' "decGeneric" "leftovers" "" (c : cs)) $ left' liftCE ((,cs) <$> conv [c])
  [] -> failDE "decGeneric" "no data" "" []

-- | xlates a vinyl record of decoders and returns a single decoder on a vinyl list [sequence]
type DecW :: [Type] -> Constraint
class DecW rs where
  decW :: Rec Dec rs -> Dec (Rec V.Identity rs)

instance DecW '[] where
  decW RNil = Dec $ \xs -> Right (RNil, xs)
instance DecW rs => DecW (r ': rs) where
  decW (d :& ds) = (\a b -> V.Identity a :& b) <$> d <*> decW ds

-- | given a vinyl record of decoders will generate a decoder for a record of ElFields
type DecH :: [(Symbol, Type)] -> Constraint
class DecH rs where
  decH :: Rec Dec (TP.Snds rs) -> Dec (Rec ElField rs)

instance DecH '[] where
  decH RNil = Dec $ \xs -> Right (RNil, xs)
instance (KnownSymbol s, DecH rs) => DecH ('(s, t) ': rs) where
  decH (d :& ds) = (\a b -> V.Field @s a :& b) <$> d <*> decH ds

-- we dont need to specify rs as ElFieldDec drives inference

-- | given a vinyl record of elfield decoders will generate a decoder for a record of ElFields
type DecI :: [(Symbol, Type)] -> Constraint
class DecI rs where
  decI :: Rec ElFieldDec rs -> Dec (Rec ElField rs)

instance DecI '[] where
  decI RNil = Dec $ \xs -> Right (RNil, xs)
instance (KnownSymbol s, DecI rs) => DecI ('(s, t) ': rs) where
  decI (FieldDec (d :: Dec t) :& ds) = (\a b -> V.Field @s a :& b) <$> d <*> decI ds

-- | simple wrapper around 'ElField' that holds a decoder
data ElFieldDec (field :: (Symbol, Type)) where
  FieldDec :: KnownSymbol s => !(Dec t) -> ElFieldDec '(s, t)

-- | convenience operator for accessing a label in a 'ElField'
(==:) :: KnownSymbol l => Label l -> Dec v -> ElFieldDec (l ::: v)
(==:) _ = FieldDec

infix 4 ==:

-- | 'DecList' takes a vinyl record of decoders and converts to a decoder of an ntuple
type DecList :: [Type] -> Constraint
class DecList rs where
  decList :: HasCallStack => Rec Dec rs -> Dec (DecTuples rs)

instance GL.TypeError ( 'GL.Text "DecList not defined for the empty list") => DecList '[] where
  decList RNil = programmerError "decList empty case!"
instance DecList '[d1] where
  decList (d1 :& RNil) = One <$> d1
instance DecList '[d1, d2] where
  decList (d1 :& d2 :& RNil) = (,) <$> d1 <*> d2
instance DecList '[d1, d2, d3] where
  decList (d1 :& d2 :& d3 :& RNil) = (,,) <$> d1 <*> d2 <*> d3
instance DecList '[d1, d2, d3, d4] where
  decList (d1 :& d2 :& d3 :& d4 :& RNil) = (,,,) <$> d1 <*> d2 <*> d3 <*> d4
instance DecList '[d1, d2, d3, d4, d5] where
  decList (d1 :& d2 :& d3 :& d4 :& d5 :& RNil) = (,,,,) <$> d1 <*> d2 <*> d3 <*> d4 <*> d5
instance DecList '[d1, d2, d3, d4, d5, d6] where
  decList (d1 :& d2 :& d3 :& d4 :& d5 :& d6 :& RNil) = (,,,,,) <$> d1 <*> d2 <*> d3 <*> d4 <*> d5 <*> d6
instance DecList '[d1, d2, d3, d4, d5, d6, d7] where
  decList (d1 :& d2 :& d3 :& d4 :& d5 :& d6 :& d7 :& RNil) = (,,,,,,) <$> d1 <*> d2 <*> d3 <*> d4 <*> d5 <*> d6 <*> d7
instance DecList '[d1, d2, d3, d4, d5, d6, d7, d8] where
  decList (d1 :& d2 :& d3 :& d4 :& d5 :& d6 :& d7 :& d8 :& RNil) = (,,,,,,,) <$> d1 <*> d2 <*> d3 <*> d4 <*> d5 <*> d6 <*> d7 <*> d8
instance DecList '[d1, d2, d3, d4, d5, d6, d7, d8, d9] where
  decList (d1 :& d2 :& d3 :& d4 :& d5 :& d6 :& d7 :& d8 :& d9 :& RNil) = (,,,,,,,,) <$> d1 <*> d2 <*> d3 <*> d4 <*> d5 <*> d6 <*> d7 <*> d8 <*> d9
instance DecList '[d1, d2, d3, d4, d5, d6, d7, d8, d9, d10] where
  decList (d1 :& d2 :& d3 :& d4 :& d5 :& d6 :& d7 :& d8 :& d9 :& d10 :& RNil) = (,,,,,,,,,) <$> d1 <*> d2 <*> d3 <*> d4 <*> d5 <*> d6 <*> d7 <*> d8 <*> d9 <*> d10

-- | convert a hlist of decoders to a tuple type
type family DecTuples rs = z | z -> rs where
--  DecTuples '[] = GL.TypeError ('GL.Text "no decoding defined for empty list")
  DecTuples '[d1] = One d1
  DecTuples '[d1, d2] = (d1, d2)
  DecTuples '[d1, d2, d3] = (d1, d2, d3)
  DecTuples '[d1, d2, d3, d4] = (d1, d2, d3, d4)
  DecTuples '[d1, d2, d3, d4, d5] = (d1, d2, d3, d4, d5)
  DecTuples '[d1, d2, d3, d4, d5, d6] = (d1, d2, d3, d4, d5, d6)
  DecTuples '[d1, d2, d3, d4, d5, d6, d7] = (d1, d2, d3, d4, d5, d6, d7)
  DecTuples '[d1, d2, d3, d4, d5, d6, d7, d8] = (d1, d2, d3, d4, d5, d6, d7, d8)
  DecTuples '[d1, d2, d3, d4, d5, d6, d7, d8, d9] = (d1, d2, d3, d4, d5, d6, d7, d8, d9)
  DecTuples '[d1, d2, d3, d4, d5, d6, d7, d8, d9, d10] = (d1, d2, d3, d4, d5, d6, d7, d8, d9, d10)

-- | 'defD' is a convenience method for defDec using typeapplications
defD :: forall a. DefDec (Dec a) => Dec a
defD = defDec

-- | 'DefDec' returns a default decoder for a type
type DefDec :: Type -> Constraint
class DefDec a where
  defDec :: HasCallStack => a
  default defDec :: (Conv b, Dec b ~ a) => a
  defDec = decGeneric

instance DefDec (Rec f '[]) where
  defDec = RNil

-- does this instance make sense?
-- todo: add messages to indicate that it was from V.Identity
instance DefDec (Dec a) => DefDec (Dec (V.Identity a)) where
  defDec = V.Identity <$> defDec @(Dec a)

instance DefDec (Dec a) => DefDec (Dec (L.Identity a)) where
  defDec = L.Identity <$> defDec @(Dec a)

-- used for Rec Dec '[Sel a, Sel b] -- ie f == Dec
instance (DefDec (f t), DefDec (Rec f ts)) => DefDec (Rec f (t ': ts)) where
  defDec = defDec :& defDec

instance (DefDec (Dec (f t)), DefDec (Dec (Rec f ts))) => DefDec (Dec (Rec f (t ': ts))) where
  defDec = (:&) <$> (defDec :: Dec (f t)) <*> (defDec :: Dec (Rec f ts))

instance DefDec (Dec (Rec f '[])) where
  defDec = Dec $ \x -> Right (RNil, x)

-- | 'mapDecMessage' prefix DE error message with some text if present: just adds more context to an existing error message
mapDecMessage :: String -> CoRec V.Identity DE' -> CoRec V.Identity DE'
mapDecMessage s de =
  VC.match de $
    VC.H (\e -> CoRec $ V.Identity $ e{cvMessage = s ++ ":" ++ cvMessage e})
      :& VC.H (\e -> CoRec $ V.Identity $ e{deMethod = s ++ ":" ++ deMethod e})
      :& RNil

-- | extract the conversion or decoding error from 'DE''
getDecError :: CoRec V.Identity DE' -> Either ConvE DecodingE
getDecError co =
  VC.match co $
    VC.H Left
      :& VC.H Right
      :& RNil

-- | extract the conversion or decoding errors from 'DE'
getDecErrors :: DE -> These (N.NonEmpty ConvE) (N.NonEmpty DecodingE)
getDecErrors de =
  partitionEithersNE $ getDecError <$> de

instance
  (DefDec (Dec t), KnownSymbol s) =>
  DefDec (Dec (ElField (s ::: t)))
  where
  defDec = Dec $ \ss -> case unDec (defDec @(Dec t)) ss of
    Left de -> Left $ fmap (mapDecMessage ("ElField " ++ symbolVal (Proxy @s))) de
    Right (a, ss') -> Right (Field a, ss')

instance DefDec (Dec a) => DefDec (Dec (DM a)) where
  defDec = DM . pure <$> defDec

instance
  GL.TypeError
    ( 'GL.Text "I do not know what type you want"
        ':$$: 'GL.Text "somehow GHC has bounced you here: DefDec (Dec ())"
    ) =>
  DefDec (Dec ())
  where
  defDec = programmerError "defDec: unreachable"

-- | failure decoder for a conversion or decoder error
decFail :: String -> String -> String -> Dec a
decFail e s x = Dec (failDE e s x)

{-
instance
  ( Typeable i
  , R2.Refined2C opts ip op i
  , DefDec (Dec i)
  , Show (PP ip i)
  ) =>
  DefDec (Dec (R2.Refined2 opts ip op i))
  where
  defDec =
    let nm = "Refined2"
        msg = show (typeRep (Proxy @i)) ++ " decoder failed: it is the input to " ++ nm
     in decAddError nm msg "" (defDec @(Dec i))
          >>= \a -> case R2.newRefined2 @opts @ip @op @i a of
            Left m2 ->
              decFail
                (nm <> " " <> R2.m2Desc m2 <> " | " <> R2.m2Short m2)
                ("\n" ++ R2.m2Long m2 ++ "\n")
                (psiS m2)
            Right r -> return r

-- decode the output fmt stuff: need to display the errors better
-- use or create new combinators to stop going inside Dec all the time: we have monads functors etc
instance
  ( Typeable i
  , R3.Refined3C opts ip op fmt i
  , DefDec (Dec i)
  , Show (PP ip i)
  ) =>
  DefDec (Dec (R3.Refined3 opts ip op fmt i))
  where
  defDec =
    let nm = "Refined3"
        msg = show (typeRep (Proxy @i)) ++ " decoder failed: it is the input to " ++ nm
     in decAddError nm msg "" (defDec @(Dec i))
          >>= \a -> case R3.newRefined3 @opts @ip @op @fmt @i a of
            Left m3 ->
              decFail
                (nm <> " " <> R3.m3Desc m3 <> " | " <> R3.m3Short m3)
                ("\n" ++ R3.m3Long m3 ++ "\n")
                (psiS m3)
            Right r -> return r

instance
  ( Typeable i
  , RefinedC opts p i
  , DefDec (Dec i)
  ) =>
  DefDec (Dec (Refined opts p i))
  where
  defDec =
    let nm = "Refined"
        msg = show (typeRep (Proxy @i)) ++ " decoder failed: it is the input to " ++ nm
     in decAddError nm msg "" (defDec @(Dec i))
          >>= \i -> case newRefined @opts @p @i i of
            Left (Msg0 _bp top e bpc) ->
              decFail
                (nm <> " " <> bpc <> " " <> top)
                ("\n" ++ e ++ "\n")
                (psiS e)
            Right r -> return r
-}
instance DefDec (Dec a) => DefDec (Dec (One a)) where
  defDec = One <$> defDec

instance DefDec (Dec [SqlValue]) where
  defDec = Dec $ \xs -> Right (xs, [])

instance DefDec (Dec Raw) where
  defDec = Raw <$> defDec

instance DefDec (Dec ByteString)
instance DefDec (Dec SqlValue)
instance DefDec (Dec Int)
instance DefDec (Dec Bool)
instance DefDec (Dec Char)
instance DefDec (Dec String)
instance DefDec (Dec Text)
instance DefDec (Dec Integer)
instance DefDec (Dec Float)
instance DefDec (Dec Double)
instance DefDec (Dec Day)
instance DefDec (Dec UTCTime)
instance DefDec (Dec LocalTime)

-- could write in terms of decMaybe but this is good as is and direct
instance DefDec (Dec a) => DefDec (Dec (Maybe a)) where
  defDec = Dec $ \hs -> left' (decAddError' "DefDec (Dec Maybe)" "" "" hs) $
    case hs of
      SqlNull : cs -> Right (Nothing, cs)
      cs -> unDec (Just <$> defDec) cs

-- | catches parse errors but expects that only one field was parsed
instance DefDec (Dec a) => DefDec (Dec (Either String a)) where
  defDec = left psiS <$> defDec @(Dec (Either DE a))

-- | assumes that we will consume one value only
instance DefDec (Dec a) => DefDec (Dec (Either DE a)) where
  defDec = Dec $ \hs -> left' (decAddError' "DefDec (Dec Either)" "" "" hs) $
    case hs of
      [] -> failDE "Either" "no data" "" hs
      c : cs -> case unDec (defDec @(Dec a)) [c] of
        Left e -> Right (Left e, cs)
        Right (a, []) -> Right (Right a, cs)
        Right (_a, _ : _) -> failDE "Either" "consumed no data and yet returned a value!" "" hs

-- | like 'Either' but also holds number of elements in "n"
newtype EitherN n e a = EitherN (Either e a) deriving stock (Functor, Generic)

-- | a more generic version of Either that allows you to specify how many fields are expected to be decoded
instance (KnownNat n, DefDec (Dec a)) => DefDec (Dec (EitherN n DE a)) where
  defDec =
    let n = fromIntegral (natVal (Proxy @n))
        nm = "EitherN(" ++ show n ++ ")"
     in Dec $ \hs -> left' (decAddError' ("DefDec (Dec " ++ nm ++ ")") "" "" hs) $
          case splitAt n hs of
            (xs, ys)
              | length xs < n -> failDE nm ("not enough data need >=" ++ show n ++ " but found " ++ show (length xs)) "" hs
              | otherwise ->
                  case unDec (defDec @(Dec a)) xs of
                    Left e -> Right (EitherN $ Left e, ys)
                    Right (a, []) -> Right (EitherN $ Right a, ys)
                    Right (_a, leftovers@(_ : _)) -> failDE nm ("didnt consume all the data it was given but returned a value anyway!\n" ++ psiS leftovers) "" hs

instance
  ( KnownNat n
  , DefDec (Dec a)
  , TP.FailUnless
      (1 GL.<=? n)
      ( 'GL.Text "Exact n a: requires n >= 1 but found n="
          ':<>: 'GL.ShowType n
          ':$$: 'GL.Text "  a="
          ':<>: 'GL.ShowType a
      )
  ) =>
  DefDec (Dec (DecNE n a))
  where
  defDec = decNE defDec

-- | decoder for a non empty list of decoders
decNE ::
  forall n a.
  KnownNat n =>
  Dec a ->
  Dec (DecNE n a)
decNE (Dec d) =
  let n = fromIntegral (natVal (Proxy @n))
      nm = "Dec (DecNE " ++ show n ++ ")"
   in Dec $ \hhs -> left' (decAddError' nm "" "" hhs) $ do
        (as, lft) <- runStateT (replicateM n (StateT d)) hhs
        case as of
          [] -> failDE nm "expected at least one decoder" "" hhs
          x : xs -> return (DecNE (x :| xs), lft)

instance (KnownNat n, DefDec (Dec a)) => DefDec (Dec (DecN n a)) where
  defDec = decN defDec

-- | creates a decoder for a fixed number of decoders given by "n"
decN :: forall n a. KnownNat n => Dec a -> Dec (DecN n a)
decN (Dec d) =
  let n = fromIntegral (natVal (Proxy @n))
   in Dec $ \hhs -> left' (decAddError' ("Dec (DecN " ++ show n ++ ")") "" "" hhs) $ do
        (as, lft) <- runStateT (replicateM n (StateT d)) hhs
        return (DecN as, lft)

instance DefDec (Dec a) => DefDec (Dec (DecAlle a)) where
  defDec = decAlle defDec

instance (DefDec (Dec a1), DefDec (Dec a2)) => DefDec (Dec (a1, a2)) where
  defDec = (,) <$> defDec <*> defDec
instance (DefDec (Dec a1), DefDec (Dec a2), DefDec (Dec a3)) => DefDec (Dec (a1, a2, a3)) where
  defDec = (,,) <$> defDec <*> defDec <*> defDec
instance (DefDec (Dec a1), DefDec (Dec a2), DefDec (Dec a3), DefDec (Dec a4)) => DefDec (Dec (a1, a2, a3, a4)) where
  defDec = (,,,) <$> defDec <*> defDec <*> defDec <*> defDec
instance (DefDec (Dec a1), DefDec (Dec a2), DefDec (Dec a3), DefDec (Dec a4), DefDec (Dec a5)) => DefDec (Dec (a1, a2, a3, a4, a5)) where
  defDec = (,,,,) <$> defDec <*> defDec <*> defDec <*> defDec <*> defDec
instance (DefDec (Dec a1), DefDec (Dec a2), DefDec (Dec a3), DefDec (Dec a4), DefDec (Dec a5), DefDec (Dec a6)) => DefDec (Dec (a1, a2, a3, a4, a5, a6)) where
  defDec = (,,,,,) <$> defDec <*> defDec <*> defDec <*> defDec <*> defDec <*> defDec
instance (DefDec (Dec a1), DefDec (Dec a2), DefDec (Dec a3), DefDec (Dec a4), DefDec (Dec a5), DefDec (Dec a6), DefDec (Dec a7)) => DefDec (Dec (a1, a2, a3, a4, a5, a6, a7)) where
  defDec = (,,,,,,) <$> defDec <*> defDec <*> defDec <*> defDec <*> defDec <*> defDec <*> defDec
instance (DefDec (Dec a1), DefDec (Dec a2), DefDec (Dec a3), DefDec (Dec a4), DefDec (Dec a5), DefDec (Dec a6), DefDec (Dec a7), DefDec (Dec a8)) => DefDec (Dec (a1, a2, a3, a4, a5, a6, a7, a8)) where
  defDec = (,,,,,,,) <$> defDec <*> defDec <*> defDec <*> defDec <*> defDec <*> defDec <*> defDec <*> defDec
instance (DefDec (Dec a1), DefDec (Dec a2), DefDec (Dec a3), DefDec (Dec a4), DefDec (Dec a5), DefDec (Dec a6), DefDec (Dec a7), DefDec (Dec a8), DefDec (Dec a9)) => DefDec (Dec (a1, a2, a3, a4, a5, a6, a7, a8, a9)) where
  defDec = (,,,,,,,,) <$> defDec <*> defDec <*> defDec <*> defDec <*> defDec <*> defDec <*> defDec <*> defDec <*> defDec
instance (DefDec (Dec a1), DefDec (Dec a2), DefDec (Dec a3), DefDec (Dec a4), DefDec (Dec a5), DefDec (Dec a6), DefDec (Dec a7), DefDec (Dec a8), DefDec (Dec a9), DefDec (Dec a10)) => DefDec (Dec (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)) where
  defDec = (,,,,,,,,,) <$> defDec <*> defDec <*> defDec <*> defDec <*> defDec <*> defDec <*> defDec <*> defDec <*> defDec <*> defDec

-- | default decoder for a record with 2 fields
defD2 ::
  (DefDec (f a1), DefDec (f a2), Applicative f) =>
  (a1 -> a2 -> ret) ->
  f ret
defD2 x = x <$> defDec <*> defDec

-- | default decoder for a record with 3 fields
defD3 ::
  (DefDec (f a1), DefDec (f a2), DefDec (f a3), Applicative f) =>
  (a1 -> a2 -> a3 -> ret) ->
  f ret
defD3 x = x <$> defDec <*> defDec <*> defDec

-- | default decoder for a record with 4 fields
defD4 ::
  (DefDec (f a1), DefDec (f a2), DefDec (f a3), DefDec (f a4), Applicative f) =>
  (a1 -> a2 -> a3 -> a4 -> ret) ->
  f ret
defD4 x = x <$> defDec <*> defDec <*> defDec <*> defDec

-- | default decoder for a record with 5 fields
defD5 ::
  (DefDec (f a1), DefDec (f a2), DefDec (f a3), DefDec (f a4), DefDec (f a5), Applicative f) =>
  (a1 -> a2 -> a3 -> a4 -> a5 -> ret) ->
  f ret
defD5 x = x <$> defDec <*> defDec <*> defDec <*> defDec <*> defDec

-- | default decoder for a record with 6 fields
defD6 ::
  (DefDec (f a1), DefDec (f a2), DefDec (f a3), DefDec (f a4), DefDec (f a5), DefDec (f a6), Applicative f) =>
  (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> ret) ->
  f ret
defD6 x = x <$> defDec <*> defDec <*> defDec <*> defDec <*> defDec <*> defDec

-- | default decoder for a record with 7 fields
defD7 ::
  (DefDec (f a1), DefDec (f a2), DefDec (f a3), DefDec (f a4), DefDec (f a5), DefDec (f a6), DefDec (f a7), Applicative f) =>
  (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> ret) ->
  f ret
defD7 x = x <$> defDec <*> defDec <*> defDec <*> defDec <*> defDec <*> defDec <*> defDec

-- | default decoder for a record with 8 fields
defD8 ::
  (DefDec (f a1), DefDec (f a2), DefDec (f a3), DefDec (f a4), DefDec (f a5), DefDec (f a6), DefDec (f a7), DefDec (f a8), Applicative f) =>
  (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> ret) ->
  f ret
defD8 x = x <$> defDec <*> defDec <*> defDec <*> defDec <*> defDec <*> defDec <*> defDec <*> defDec

-- | default decoder for a record with 9 fields
defD9 ::
  (DefDec (f a1), DefDec (f a2), DefDec (f a3), DefDec (f a4), DefDec (f a5), DefDec (f a6), DefDec (f a7), DefDec (f a8), DefDec (f a9), Applicative f) =>
  (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> ret) ->
  f ret
defD9 x = x <$> defDec <*> defDec <*> defDec <*> defDec <*> defDec <*> defDec <*> defDec <*> defDec <*> defDec

-- | default decoder for a record with 10 fields
defD10 ::
  (DefDec (f a1), DefDec (f a2), DefDec (f a3), DefDec (f a4), DefDec (f a5), DefDec (f a6), DefDec (f a7), DefDec (f a8), DefDec (f a9), DefDec (f a10), Applicative f) =>
  (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10 -> ret) ->
  f ret
defD10 x = x <$> defDec <*> defDec <*> defDec <*> defDec <*> defDec <*> defDec <*> defDec <*> defDec <*> defDec <*> defDec
