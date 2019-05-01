{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS -Wall #-}
{- |
Module      : Encoding
Description : encode from a haskell value to 'SqlValue's
Copyright   : (c) Grant Weyburne, 2016
License     : GPL-3
Maintainer  : gbwey9@gmail.com

'Enc' defines an encoder and 'DefEnc' has the default encoder for a given type
-}
module Encoding where
import Data.ByteString (ByteString)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Void
import Text.Printf
import Data.Time
import Database.HDBC (SqlValue(..))
import GHC.Generics (Generic)
import Data.Vinyl
import qualified Data.Vinyl.Functor as V
import qualified Data.Vinyl.Recursive as VR
import qualified Control.Lens as L
import VinylUtils
import Data.Function

-- | 'Enc' encodes a haskell value to a list of sqlvalues
newtype Enc a = Enc { unEnc :: a -> [SqlValue] } deriving Generic

newtype RawEnc = RawEnc { unRawEnc :: [SqlValue] } deriving (Show, Generic)

instance Contravariant Enc where
  contramap f (Enc g) = Enc (g . f)

-- doesnt make sense
--instance Monoid (Enc a) where
--  mempty = Enc (const [])

instance Semigroup (Enc a) where
  Enc p <> Enc q = Enc (p <> q)

instance Divisible Enc where
  conquer = Enc (const [])
  divide f (Enc ax) (Enc bx) = Enc $ \z -> let (a,b) = f z in ax a <> bx b

instance Decidable Enc where
 lose f = Enc $ \a -> absurd (f a)
 choose f (Enc ax) (Enc bx) = Enc $ \z -> either ax bx (f z)

encSqlValue :: Enc SqlValue
encSqlValue = Enc (:[])

encRawEnc :: Enc RawEnc
encRawEnc = Enc unRawEnc

encMaybe :: Enc a -> Enc (Maybe a)
encMaybe (Enc enc) = Enc $ \mb -> maybe [SqlNull] enc mb

-- | 'EncList' is a way to encode a list of 'a'
newtype EncList a = EncList { unEncList :: [a] } deriving Show

instance Monoid  (EncList a) where
  mempty = EncList mempty

instance Semigroup (EncList a) where
  (<>) = (EncList .) . on (<>) unEncList

encList :: Enc a -> Enc (EncList a)
encList (Enc enc) = Enc $ \(EncList as) -> concatMap enc as

encList' :: Enc a -> Enc [a]
encList' (Enc enc) = Enc (concatMap enc)

encodeList :: Enc a -> [a] -> [SqlValue]
encodeList = unEnc . encList'

-- can use this with eg I1 directly
encodeList' :: Enc a -> [a] -> EncList SqlValue
encodeList' = (EncList .) . encodeList

encodeListV :: Enc a -> [a] -> Rec V.Identity '[EncList SqlValue]
encodeListV = (I1 .) . encodeList'

encodeListDef :: DefEnc (Enc a) => [a] -> EncList SqlValue
encodeListDef = encodeList' defEnc

encodeListDefV :: DefEnc (Enc a) => [a] -> Rec V.Identity '[EncList SqlValue]
encodeListDefV = I1 . encodeListDef

encConst :: SqlValue -> Enc a  -- not used?
encConst = Enc . const . (:[])

encByteString :: Enc ByteString
encByteString = Enc $ \bs -> [SqlByteString bs]

encInt :: Enc Int
encInt = Enc $ \i -> [SqlInt32 (fromIntegral i)]

encInteger :: Enc Integer
encInteger = Enc $ \i -> [SqlInteger i]

-- | encoding variant for mssql
encBoolMS :: Enc Bool
encBoolMS = Enc $ \b -> [SqlChar (if b then '\1' else '\0')]

encBool :: Enc Bool
encBool = Enc $ \b -> [SqlInt32 (if b then 1 else 0)]

-- for databases that support real booleans SqlBool eg postgres. encBool will work fine in context
newtype Bool' = Bool' { unBool' :: Bool } deriving (Show,Eq)
encBool' :: Enc Bool'
encBool' = Enc ((:[]) . SqlBool . unBool')

encChar :: Enc Char
encChar = Enc $ \c -> [SqlChar c]

encString :: Enc String
encString = Enc $ \s -> [SqlString s]

encDouble :: Enc Double
encDouble = Enc $ \d -> [SqlDouble d]


encLocalTime :: Enc LocalTime
encLocalTime = Enc $ \tm -> [SqlLocalTime tm]

encDay :: Enc Day
encDay = Enc $ \dt -> [SqlLocalDate dt]

encUTCTime :: Enc UTCTime
encUTCTime = Enc $ \tm -> [SqlUTCTime tm]

encYMD :: Enc (Integer, Integer, Integer)
encYMD = Enc $ \(y,m,d) -> [SqlLocalDate (fromGregorian y (fromInteger m) (fromInteger d))]

encHMS :: Enc (Integer, Integer, Integer)
encHMS = Enc $ \(h,m,s) -> [SqlString (printf "%02i:%02i:%02i" h m s)]

encHMSRev :: Enc Integer
encHMSRev = Enc $ \i  ->
  let (r,s) = quotRem (abs i) 60
      (h,m) = quotRem r 60
  in [SqlInteger h, SqlInteger m, SqlInteger s]

instance Show (Enc a) where
  show Enc {} = "Enc<fn>"

-- saves having to type defEnc @(Enc String) == defE @String
-- | 'defE' is a convenience method for defDec using typeapplications
defE :: forall a. DefEnc (Enc a) => Enc a
defE = defEnc

-- | 'DefEnc' is the default class for encoding input
-- this will mostly do the right thing and return the right encoder
class DefEnc a where
  defEnc :: a

instance DefEnc (Enc ()) where
  defEnc = conquer
instance DefEnc (Enc ByteString) where
  defEnc = encByteString
instance DefEnc (Enc Int) where
  defEnc = encInt
instance DefEnc (Enc Integer) where
  defEnc = encInteger
instance DefEnc (Enc Float) where
  defEnc = realToFrac >$< encDouble
instance DefEnc (Enc Double) where
  defEnc = encDouble
instance DefEnc (Enc Bool) where
  defEnc = encBool
instance DefEnc (Enc Char) where
  defEnc = encChar
instance DefEnc (Enc String) where
  defEnc = encString
instance DefEnc (Enc Text) where
  defEnc = T.unpack >$< encString
instance DefEnc (Enc UTCTime) where
  defEnc = encUTCTime
instance DefEnc (Enc ZonedTime) where
  defEnc = zonedTimeToLocalTime >$< encLocalTime
instance DefEnc (Enc LocalTime) where
  defEnc = encLocalTime
instance DefEnc (Enc Day) where
  defEnc = encDay

instance DefEnc (Enc RawEnc) where
  defEnc = encRawEnc
instance DefEnc (Enc SqlValue) where
  defEnc = encSqlValue

instance DefEnc (Enc a) => DefEnc (Enc (Maybe a)) where
  defEnc = encMaybe defEnc

instance DefEnc (Enc a) => DefEnc (Enc (EncList a)) where
  defEnc = unEncList >$< encList' defEnc

instance (DefEnc (Enc a1),DefEnc (Enc a2)) => DefEnc (Enc (a1,a2)) where
  defEnc = divided defEnc defEnc
instance (DefEnc (Enc a1),DefEnc (Enc a2),DefEnc (Enc a3)) => DefEnc (Enc (a1,a2,a3)) where
  defEnc = divide (\(a1,a2,a3) -> (a1,(a2,a3))) defEnc defEnc
instance (DefEnc (Enc a1),DefEnc (Enc a2),DefEnc (Enc a3),DefEnc (Enc a4)) => DefEnc (Enc (a1,a2,a3,a4)) where
  defEnc = divide (\(a1,a2,a3,a4) -> (a1,(a2,a3,a4))) defEnc defEnc
instance (DefEnc (Enc a1),DefEnc (Enc a2),DefEnc (Enc a3),DefEnc (Enc a4),DefEnc (Enc a5)) => DefEnc (Enc (a1,a2,a3,a4,a5)) where
  defEnc = divide (\(a1,a2,a3,a4,a5) -> (a1,(a2,a3,a4,a5))) defEnc defEnc
instance (DefEnc (Enc a1),DefEnc (Enc a2),DefEnc (Enc a3),DefEnc (Enc a4),DefEnc (Enc a5),DefEnc (Enc a6)) => DefEnc (Enc (a1,a2,a3,a4,a5,a6)) where
  defEnc = divide (\(a1,a2,a3,a4,a5,a6) -> (a1,(a2,a3,a4,a5,a6))) defEnc defEnc
instance (DefEnc (Enc a1),DefEnc (Enc a2),DefEnc (Enc a3),DefEnc (Enc a4),DefEnc (Enc a5),DefEnc (Enc a6),DefEnc (Enc a7)) => DefEnc (Enc (a1,a2,a3,a4,a5,a6,a7)) where
  defEnc = divide (\(a1,a2,a3,a4,a5,a6,a7) -> (a1,(a2,a3,a4,a5,a6,a7))) defEnc defEnc
instance (DefEnc (Enc a1),DefEnc (Enc a2),DefEnc (Enc a3),DefEnc (Enc a4),DefEnc (Enc a5),DefEnc (Enc a6),DefEnc (Enc a7),DefEnc (Enc a8)) => DefEnc (Enc (a1,a2,a3,a4,a5,a6,a7,a8)) where
  defEnc = divide (\(a1,a2,a3,a4,a5,a6,a7,a8) -> (a1,(a2,a3,a4,a5,a6,a7,a8))) defEnc defEnc
instance (DefEnc (Enc a1),DefEnc (Enc a2),DefEnc (Enc a3),DefEnc (Enc a4),DefEnc (Enc a5),DefEnc (Enc a6),DefEnc (Enc a7),DefEnc (Enc a8),DefEnc (Enc a9)) => DefEnc (Enc (a1,a2,a3,a4,a5,a6,a7,a8,a9)) where
  defEnc = divide (\(a1,a2,a3,a4,a5,a6,a7,a8,a9) -> (a1,(a2,a3,a4,a5,a6,a7,a8,a9))) defEnc defEnc
instance (DefEnc (Enc a1),DefEnc (Enc a2),DefEnc (Enc a3),DefEnc (Enc a4),DefEnc (Enc a5),DefEnc (Enc a6),DefEnc (Enc a7),DefEnc (Enc a8),DefEnc (Enc a9),DefEnc (Enc a10)) => DefEnc (Enc (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10)) where
  defEnc = divide (\(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) -> (a1,(a2,a3,a4,a5,a6,a7,a8,a9,a10))) defEnc defEnc
instance (DefEnc (Enc a1),DefEnc (Enc a2),DefEnc (Enc a3),DefEnc (Enc a4),DefEnc (Enc a5),DefEnc (Enc a6),DefEnc (Enc a7),DefEnc (Enc a8),DefEnc (Enc a9),DefEnc (Enc a10),DefEnc (Enc a11)) => DefEnc (Enc (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11)) where
  defEnc = divide (\(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) -> (a1,(a2,a3,a4,a5,a6,a7,a8,a9,a10,a11))) defEnc defEnc

instance DefEnc (Rec f '[]) where
  defEnc = RNil

instance (DefEnc (f t), DefEnc (Rec f ts)) => DefEnc (Rec f (t ': ts)) where
  defEnc = defEnc :& defEnc

instance DefEnc (Enc (Rec f '[])) where
  defEnc = Enc $ \RNil -> []

instance (DefEnc (Enc (f t)), DefEnc (Enc (Rec f ts))) => DefEnc (Enc (Rec f (t ': ts))) where
--  defEnc = Enc $ \(r :& rs) -> unEnc (defEnc @(Enc (f t))) r <> unEnc (defEnc @(Enc (Rec f ts))) rs
  defEnc = Enc $ \(r :& rs) -> unEnc defEnc r <> unEnc defEnc rs

-- meaningless: has to be Enc!
--instance DefEnc a => DefEnc (V.Identity a) where
--  defEnc = V.Identity defEnc

--instance DefEnc a => DefEnc (L.Identity a) where
--  defEnc = L.Identity defEnc

instance DefEnc (Enc a) => DefEnc (Enc (V.Identity a)) where
  defEnc = contramap V.getIdentity (defEnc @(Enc a))

instance DefEnc (Enc a) => DefEnc (Enc (L.Identity a)) where
  defEnc = contramap L.runIdentity defEnc

encodeVals :: Rec Enc rs -> Rec V.Identity rs -> [SqlValue]
encodeVals encRec vals = VR.rfoldMap V.getConst $ VR.rzipWith (\(Enc f) (V.Identity y) -> V.Const (f y)) encRec vals

encode1 :: DefEnc (Enc a) => a -> [SqlValue]
encode1 a = encodeVals defEnc (I1 a)

encode2 :: (DefEnc (Enc a), DefEnc (Enc b)) => (a,b) -> [SqlValue]
encode2 (a,b) = encodeVals defEnc (I2 a b)

encode3 :: (DefEnc (Enc a), DefEnc (Enc b), DefEnc (Enc c)) => (a,b,c) -> [SqlValue]
encode3 (a,b,c) = encodeVals defEnc (I3 a b c)

encode4 :: (DefEnc (Enc a), DefEnc (Enc b), DefEnc (Enc c), DefEnc (Enc d)) => (a,b,c,d) -> [SqlValue]
encode4 (a,b,c,d) = encodeVals defEnc (I4 a b c d)

