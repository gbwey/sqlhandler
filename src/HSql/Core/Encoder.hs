{-
SqlBool True -> "select ?" postgres fails unless you cast ? to the type you want

to use defEnc need 4 things
1. deriving G.Generic
2. instance GS.Generic LogCmd
3. instance GS.HasDatatypeInfo LogCmd
4. instance DefEnc (Enc LogCmd)
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : HSql.Core.Encoder
Description : encode from a haskell value to 'SqlValue's
Copyright   : (c) Grant Weyburne, 2016
License     : BSD-3

'Enc' defines an encoder and 'DefEnc' has the default encoder for a given type. Used for converting input from haskell values to sql values.
-}
module HSql.Core.Encoder where

import Control.DeepSeq (NFData)
import qualified Control.Lens as L
import Data.ByteString (ByteString)
import Data.Coerce
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Vinyl
import qualified Data.Vinyl.Functor as V
import qualified Data.Vinyl.Recursive as VR
import Data.Void
import Database.HDBC (SqlValue (..))
import GHC.Generics (Generic)
import qualified Generics.OneLiner as GO
import HSql.Core.Raw
import HSql.Core.VinylUtils (pattern I1, pattern I2, pattern I3, pattern I4)

{-
import Predicate.Refined (Refined, unRefined)
import qualified Predicate.Refined2 as R2
import qualified Predicate.Refined3 as R3
-}
import Text.Printf (printf)

-- | 'Enc' encodes a haskell value to a list of sqlvalues
newtype Enc a = Enc {unEnc :: a -> [SqlValue]}
  deriving stock (Generic)
  deriving newtype (Semigroup, NFData)

-- instance NFData a => NFData (Enc a)

-- | default encoder class
class DefEnc (Enc a) => DefE a where
  defE :: Enc a
  defE = defEnc

instance DefEnc (Enc a) => DefE a

-- | creates a list of 'SqlValue' by encoding each value in a record
gencode :: forall t. (GO.ADT t, GO.Constraints t DefE) => t -> [SqlValue]
gencode = GO.gfoldMap @DefE (unEnc defE)

-- | holds raw unencoded 'SqlValue'
newtype RawEnc = RawEnc {unRawEnc :: [SqlValue]}
  deriving stock (Show, Generic)
  deriving newtype (NFData)

instance Contravariant Enc where
  contramap f (Enc g) = Enc (g . f)

instance Divisible Enc where
  conquer = Enc (const [])
  divide f (Enc ax) (Enc bx) = Enc $ \z -> let (a, b) = f z in ax a <> bx b

instance Decidable Enc where
  lose f = Enc $ \a -> absurd (f a)
  choose f (Enc ax) (Enc bx) = Enc $ \z -> either ax bx (f z)

-- | encoder for a 'SqlValue'
encSqlValue :: Enc SqlValue
encSqlValue = Enc pure

-- | encoder for a list of 'SqlValue's
encRawEnc :: Enc RawEnc
encRawEnc = Enc unRawEnc

-- | raw encoder
encRaw :: Enc Raw
encRaw = Enc coerce

-- | encoder for 'Maybe'
encMaybe :: Enc a -> Enc (Maybe a)
encMaybe (Enc enc) = Enc $ \mb -> maybe [SqlNull] enc mb

-- | 'EncList' is a way to encode a list of "a"
newtype EncList a = EncList {unEncList :: [a]}
  deriving stock (Eq, Show, Generic, Functor)
  deriving newtype (Semigroup, Monoid)

-- | encoder for 'EncList'
encList :: Enc a -> Enc (EncList a)
encList (Enc enc) = Enc $ \(EncList as) -> concatMap enc as

-- | encoder for a list of "a"s
encList' :: Enc a -> Enc [a]
encList' (Enc enc) = Enc (concatMap enc)

-- | encode a list of "a" into a list of 'SqlValue's
encodeList :: Enc a -> [a] -> [SqlValue]
encodeList = unEnc . encList'

-- can use this with eg I1 directly

-- | encoder that takes a list of "a" and returns an list of 'SqlValue'
encodeList' :: Enc a -> [a] -> EncList SqlValue
encodeList' = (EncList .) . encodeList

-- | encoder that takes a list of "a" and returns an list of 'SqlValue' as a first entry in a vinyl list using the given encoder
encodeListV :: Enc a -> [a] -> Rec V.Identity '[EncList SqlValue]
encodeListV = (I1 .) . encodeList'

-- | encoder that takes a list of "a" and returns an list of 'SqlValue' using the default encoder
encodeListDef :: DefEnc (Enc a) => [a] -> EncList SqlValue
encodeListDef = encodeList' defEnc

-- | encoder that takes a list of "a" and returns an list of 'SqlValue' as a first entry in a vinyl list using the default encoder
encodeListDefV :: DefEnc (Enc a) => [a] -> Rec V.Identity '[EncList SqlValue]
encodeListDefV = I1 . encodeListDef

-- | encoder for a 'ByteString'
encByteString :: Enc ByteString
encByteString = Enc $ \bs -> [SqlByteString bs]

-- | encoder for a 'Int'
encInt :: Enc Int
encInt = Enc $ \i -> [SqlInt64 (fromIntegral i)]

-- | encoder for a 'Integer'
encInteger :: Enc Integer
encInteger = Enc $ \i -> [SqlInteger i]

-- | encoder for a 'Bool' specific to mssql
encBoolMS :: Enc Bool
encBoolMS = Enc $ \b -> [SqlChar (if b then '\1' else '\0')]

-- | encoder for a 'Bool'
encBool :: Enc Bool
encBool = Enc $ \b -> [SqlInt32 (if b then 1 else 0)]

-- | encoder for a 'Char'
encChar :: Enc Char
encChar = Enc $ \c -> [SqlChar c]

-- | encoder for a 'String'
encString :: Enc String
encString = Enc $ \s -> [SqlString s]

-- | encoder for a 'Double'
encDouble :: Enc Double
encDouble = Enc $ \d -> [SqlDouble d]

{-
encRefined :: DefEnc (Enc i) => Enc (Refined opts p i)
encRefined = Enc $ unEnc defEnc . unRefined

encRefined2 :: DefEnc (Enc i) => Enc (R2.Refined2 opts ip op i)
encRefined2 = Enc $ unEnc defEnc . R2.r2Out

-- do we encode the fmt output
encRefined3 :: DefEnc (Enc i) => Enc (R3.Refined3 opts ip op fmt i)
encRefined3 = Enc $ unEnc defEnc . R3.r3Out
-}

-- | encoder for a 'LocalTime'
encLocalTime :: Enc LocalTime
encLocalTime = Enc $ \tm -> [SqlLocalTime tm]

-- | encoder for a 'Day'
encDay :: Enc Day
encDay = Enc $ \dt -> [SqlLocalDate dt]

-- | encoder for a 'UTCTime'
encUTCTime :: Enc UTCTime
encUTCTime = Enc $ \tm -> [SqlUTCTime tm]

-- | encoder for a LocalDate
encYMD :: Enc (Integer, Integer, Integer)
encYMD = Enc $ \(y, m, d) -> [SqlLocalDate (fromGregorian y (fromInteger m) (fromInteger d))]

-- | encoder for a hour minutes seconds packed into a 'SqlString' field
encHMS :: Enc (Integer, Integer, Integer)
encHMS = Enc $ \(h, m, s) -> [SqlString (printf "%02i:%02i:%02i" h m s)]

-- | encoder for a hour minutes seconds using an 'Integer'
encHMSRev :: Enc Integer
encHMSRev = Enc $ \i ->
  let (r, s) = quotRem (abs i) 60
      (h, m) = quotRem r 60
   in [SqlInteger h, SqlInteger m, SqlInteger s]

instance Show (Enc a) where
  show Enc{} = "Enc<fn>"

-- | 'DefEnc' is the default class for encoding input
class DefEnc a where
  defEnc :: a
  default defEnc :: (a ~ Enc t, GO.ADT t, GO.Constraints t DefE) => a
  defEnc = Enc gencode

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

{-
instance DefEnc (Enc i) => DefEnc (Enc (Refined opts p i)) where
  defEnc = encRefined
instance DefEnc (Enc i) => DefEnc (Enc (R2.Refined2 opts ip op i)) where
  defEnc = encRefined2
-- only care about the outputted value to be encoded so dont need DefEnc (Enc (PP ip i))
instance DefEnc (Enc i) => DefEnc (Enc (R3.Refined3 opts ip op fmt i)) where
  defEnc = encRefined3
-}
instance DefEnc (Enc RawEnc) where
  defEnc = encRawEnc
instance DefEnc (Enc Raw) where
  defEnc = encRaw
instance DefEnc (Enc SqlValue) where
  defEnc = encSqlValue

instance DefEnc (Enc a) => DefEnc (Enc (Maybe a)) where
  defEnc = encMaybe defEnc

instance DefEnc (Enc a) => DefEnc (Enc (EncList a)) where
  defEnc = unEncList >$< encList' defEnc

-- only defined in one-liner for tuples of size seven but defined in SOP for higher stuff: although there are instances higher than that???
-- could inline gfoldMap and then could get rid of the one-liner dependency and use the bigger tuple sizes of sop but gfoldMap is complex
instance (DefEnc (Enc a1), DefEnc (Enc a2)) => DefEnc (Enc (a1, a2))
instance (DefEnc (Enc a1), DefEnc (Enc a2), DefEnc (Enc a3)) => DefEnc (Enc (a1, a2, a3))
instance (DefEnc (Enc a1), DefEnc (Enc a2), DefEnc (Enc a3), DefEnc (Enc a4)) => DefEnc (Enc (a1, a2, a3, a4))
instance (DefEnc (Enc a1), DefEnc (Enc a2), DefEnc (Enc a3), DefEnc (Enc a4), DefEnc (Enc a5)) => DefEnc (Enc (a1, a2, a3, a4, a5))
instance (DefEnc (Enc a1), DefEnc (Enc a2), DefEnc (Enc a3), DefEnc (Enc a4), DefEnc (Enc a5), DefEnc (Enc a6)) => DefEnc (Enc (a1, a2, a3, a4, a5, a6))
instance (DefEnc (Enc a1), DefEnc (Enc a2), DefEnc (Enc a3), DefEnc (Enc a4), DefEnc (Enc a5), DefEnc (Enc a6), DefEnc (Enc a7)) => DefEnc (Enc (a1, a2, a3, a4, a5, a6, a7))

instance (DefEnc (Enc a1), DefEnc (Enc a2), DefEnc (Enc a3), DefEnc (Enc a4), DefEnc (Enc a5), DefEnc (Enc a6), DefEnc (Enc a7), DefEnc (Enc a8)) => DefEnc (Enc (a1, a2, a3, a4, a5, a6, a7, a8)) where
  defEnc = divide (\(a1, a2, a3, a4, a5, a6, a7, a8) -> (a1, (a2, a3, a4, a5, a6, a7, a8))) defEnc defEnc
instance (DefEnc (Enc a1), DefEnc (Enc a2), DefEnc (Enc a3), DefEnc (Enc a4), DefEnc (Enc a5), DefEnc (Enc a6), DefEnc (Enc a7), DefEnc (Enc a8), DefEnc (Enc a9)) => DefEnc (Enc (a1, a2, a3, a4, a5, a6, a7, a8, a9)) where
  defEnc = divide (\(a1, a2, a3, a4, a5, a6, a7, a8, a9) -> (a1, (a2, a3, a4, a5, a6, a7, a8, a9))) defEnc defEnc
instance (DefEnc (Enc a1), DefEnc (Enc a2), DefEnc (Enc a3), DefEnc (Enc a4), DefEnc (Enc a5), DefEnc (Enc a6), DefEnc (Enc a7), DefEnc (Enc a8), DefEnc (Enc a9), DefEnc (Enc a10)) => DefEnc (Enc (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)) where
  defEnc = divide (\(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) -> (a1, (a2, a3, a4, a5, a6, a7, a8, a9, a10))) defEnc defEnc
instance (DefEnc (Enc a1), DefEnc (Enc a2), DefEnc (Enc a3), DefEnc (Enc a4), DefEnc (Enc a5), DefEnc (Enc a6), DefEnc (Enc a7), DefEnc (Enc a8), DefEnc (Enc a9), DefEnc (Enc a10), DefEnc (Enc a11)) => DefEnc (Enc (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)) where
  defEnc = divide (\(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) -> (a1, (a2, a3, a4, a5, a6, a7, a8, a9, a10, a11))) defEnc defEnc

instance DefEnc (Rec f '[]) where
  defEnc = RNil

instance (DefEnc (f t), DefEnc (Rec f ts)) => DefEnc (Rec f (t ': ts)) where
  defEnc = defEnc :& defEnc

instance DefEnc (Enc (Rec f '[])) where
  defEnc = Enc $ \RNil -> []

instance (DefEnc (Enc (f t)), DefEnc (Enc (Rec f ts))) => DefEnc (Enc (Rec f (t ': ts))) where
  --  defEnc = Enc $ \(r :& rs) -> unEnc (defEnc @(Enc (f t))) r <> unEnc (defEnc @(Enc (Rec f ts))) rs
  defEnc = Enc $ \(r :& rs) -> unEnc defEnc r <> unEnc defEnc rs

instance DefEnc (Enc a) => DefEnc (Enc (V.Identity a)) where
  defEnc = contramap V.getIdentity (defEnc @(Enc a))

instance DefEnc (Enc a) => DefEnc (Enc (L.Identity a)) where
  defEnc = contramap L.runIdentity defEnc

-- | encode values from a vinyl record "vals" using the vinyl record of encoders "encRec"
encodeVals :: Rec Enc rs -> Rec V.Identity rs -> [SqlValue]
encodeVals encRec vals = VR.rfoldMap V.getConst $ VR.rzipWith (\(Enc f) (V.Identity y) -> V.Const (f y)) encRec vals

-- | encode one value using the default encoder
encode1 :: DefEnc (Enc a) => a -> [SqlValue]
encode1 a = encodeVals defEnc (I1 a)

-- | encode two values using default encoders
encode2 :: (DefEnc (Enc a), DefEnc (Enc b)) => (a, b) -> [SqlValue]
encode2 (a, b) = encodeVals defEnc (I2 a b)

-- | encode three values using default encoders
encode3 :: (DefEnc (Enc a), DefEnc (Enc b), DefEnc (Enc c)) => (a, b, c) -> [SqlValue]
encode3 (a, b, c) = encodeVals defEnc (I3 a b c)

-- | encode four values using default encoders
encode4 :: (DefEnc (Enc a), DefEnc (Enc b), DefEnc (Enc c), DefEnc (Enc d)) => (a, b, c, d) -> [SqlValue]
encode4 (a, b, c, d) = encodeVals defEnc (I4 a b c d)
