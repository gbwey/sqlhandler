{-# OPTIONS -Wall #-}
{-# OPTIONS -Wcompat #-}
{-# OPTIONS -Wincomplete-record-updates #-}
{-# OPTIONS -Wincomplete-uni-patterns #-}
{-# OPTIONS -Wredundant-constraints #-}
{-# OPTIONS -Wunused-type-patterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |
Module      : HSql.Core.Conv
Description : Provides default converters from sql values to haskell values
Copyright   : (c) Grant Weyburne, 2016
License     : BSD-3
Maintainer  : gbwey9@gmail.com
-}
module HSql.Core.Conv where
import qualified Data.ByteString.Char8 as B
import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE (encodeUtf8,decodeUtf8)
import Data.Text (Text)
import Control.Arrow (left)
import Data.Time
import Data.Char (isSpace,ord)
import Database.HDBC (SqlValue(..))
import qualified Data.List.NonEmpty as N
import Data.List (isPrefixOf)
import HSql.Core.ErrorHandler (EE,failCE,ConvE(..))

-- | 'Conv' tries to converts one or more hdbc sql values to 'a'
class Conv a where
  conv :: [SqlValue] -> Either EE a

instance Conv SqlValue where
  conv [h] = Right h
  conv zs = failCE "SqlValue" ("expected 1 value: found " ++ show (length zs)) zs

instance Conv Bool where
  conv [b] = convbool b
  conv zs = failCE "Bool" ("expected 1 value: found " ++ show (length zs)) zs

instance Conv ByteString where
  conv [SqlByteString b] = Right b
  conv [SqlString b] = Right (TE.encodeUtf8 (T.pack b))
  conv zs@[z] = failCE "ByteString" ("from " ++ showSqlTypeHead z) zs
  conv zs  = failCE "ByteString" ("expected 1 value: found " ++ show (length zs)) zs

instance Conv String where
  conv [SqlString s] = Right s
  conv [SqlByteString bs] = Right (B.unpack bs)
  conv zs@[z] = failCE "String" ("from " ++ showSqlTypeHead z) zs
  conv zs  = failCE "String" ("expected 1 value: found " ++ show (length zs)) zs

instance Conv Text where
  conv [SqlString s] = Right (T.pack s)
  conv [SqlByteString bs] = Right (TE.decodeUtf8 bs)
  conv zs@[z] = failCE "Text" ("from " ++ showSqlTypeHead z) zs
  conv zs  = failCE "Text" ("expected 1 value: found " ++ show (length zs)) zs

instance Conv Int where
  conv [i] = convnum i
  conv zs = failCE "Int" ("expected 1 value: found " ++ show (length zs)) zs

instance Conv Integer where
  conv [i] = convnum i
  conv zs = failCE "Integer" ("expected 1 value: found " ++ show (length zs)) zs

instance Conv Char where
  conv [SqlChar c] = Right c
  conv zs@[SqlByteString bs] | Just (c,bs') <- B.uncons bs , B.null bs' = Right c
                          | otherwise = failCE "Char" "found ByteString with more than one char" zs
  conv zs@[z] = failCE "Char" ("from " ++ showSqlTypeHead z) zs
  conv zs = failCE "Char" ("expected 1 value: found " ++ show (length zs)) zs

instance Conv Float where
  conv [f] = convfloat f -- Right (realToFrac d)
  conv zs = failCE "Float" ("expected 1 value: found " ++ show (length zs)) zs

instance Conv Double where
  conv [d] = convfloat d
  conv zs = failCE "Double" ("expected 1 value: found " ++ show (length zs)) zs

instance Conv UTCTime where
  conv [SqlLocalDate a] = return $ localTimeToUTC utc (LocalTime a midnight)
  conv [SqlLocalTime a] = return $ localTimeToUTC utc a
  conv [SqlUTCTime a] = return a
  conv zs@[z] = failCE "UTCTime" ("from " ++ showSqlTypeHead z) zs
  conv zs  = failCE "UTCTime" ("expected 1 value: found " ++ show (length zs)) zs

instance Conv LocalTime where
  conv [SqlLocalDate a] = return $ LocalTime a midnight
  conv [SqlLocalTime a] = return a
  conv [SqlUTCTime a] = return $ utcToLocalTime utc a
  conv zs@[z] = failCE "LocalTime" ("from " ++ showSqlTypeHead z) zs
  conv zs  = failCE "LocalTime" ("expected 1 value: found " ++ show (length zs)) zs

instance Conv Day where
  conv [SqlLocalDate a] = return a
  conv [SqlLocalTime a] = return $ localDay a -- you lose the time info
  conv [SqlUTCTime a] = return $ utctDay a
  conv zs@[z] = failCE "Day" ("from " ++ showSqlTypeHead z) zs
  conv zs  = failCE "Day" ("expected 1 value: found " ++ show (length zs)) zs

instance Conv a => Conv (Maybe a) where
  conv [SqlNull] = Right Nothing
  conv zs@[z] = left (ConvE "(Maybe a)" ("from " ++ showSqlTypeHead z) zs N.<|) (Just <$> conv @a zs)
  conv zs    = failCE "Maybe a" ("expected 1 value: found " ++ show (length zs)) zs

instance (Conv a1,Conv a2) => Conv (a1,a2) where
  conv [a1,a2] = (,) <$> conv [a1] <*> conv [a2]
  conv zs  = failCE "tuple2" "expected 2 values" zs
instance (Conv a1,Conv a2,Conv a3) => Conv (a1,a2,a3) where
  conv [a1,a2,a3] = (,,) <$> conv [a1] <*> conv [a2] <*> conv [a3]
  conv zs  = failCE "tuple3" "expected 3 values" zs
instance (Conv a1,Conv a2,Conv a3,Conv a4) => Conv (a1,a2,a3,a4) where
  conv [a1,a2,a3,a4] = (,,,) <$> conv [a1] <*> conv [a2] <*> conv [a3] <*> conv [a4]
  conv zs  = failCE "tuple4" "expected 4 values" zs
instance (Conv a1,Conv a2,Conv a3,Conv a4,Conv a5) => Conv (a1,a2,a3,a4,a5) where
  conv [a1,a2,a3,a4,a5] = (,,,,) <$> conv [a1] <*> conv [a2] <*> conv [a3] <*> conv [a4] <*> conv [a5]
  conv zs  = failCE "tuple5" "expected 5 values" zs
instance (Conv a1,Conv a2,Conv a3,Conv a4,Conv a5,Conv a6) => Conv (a1,a2,a3,a4,a5,a6) where
  conv [a1,a2,a3,a4,a5,a6] = (,,,,,) <$> conv [a1] <*> conv [a2] <*> conv [a3] <*> conv [a4] <*> conv [a5] <*> conv [a6]
  conv zs  = failCE "tuple6" "expected 6 values" zs
instance (Conv a1,Conv a2,Conv a3,Conv a4,Conv a5,Conv a6,Conv a7) => Conv (a1,a2,a3,a4,a5,a6,a7) where
  conv [a1,a2,a3,a4,a5,a6,a7] = (,,,,,,) <$> conv [a1] <*> conv [a2] <*> conv [a3] <*> conv [a4] <*> conv [a5] <*> conv [a6] <*> conv [a7]
  conv zs  = failCE "tuple7" "expected 7 values" zs
instance (Conv a1,Conv a2,Conv a3,Conv a4,Conv a5,Conv a6,Conv a7,Conv a8) => Conv (a1,a2,a3,a4,a5,a6,a7,a8) where
  conv [a1,a2,a3,a4,a5,a6,a7,a8] = (,,,,,,,) <$> conv [a1] <*> conv [a2] <*> conv [a3] <*> conv [a4] <*> conv [a5] <*> conv [a6] <*> conv [a7] <*> conv [a8]
  conv zs  = failCE "tuple8" "expected 8 values" zs
instance (Conv a1,Conv a2,Conv a3,Conv a4,Conv a5,Conv a6,Conv a7,Conv a8,Conv a9) => Conv (a1,a2,a3,a4,a5,a6,a7,a8,a9) where
  conv [a1,a2,a3,a4,a5,a6,a7,a8,a9] = (,,,,,,,,) <$> conv [a1] <*> conv [a2] <*> conv [a3] <*> conv [a4] <*> conv [a5] <*> conv [a6] <*> conv [a7] <*> conv [a8] <*> conv [a9]
  conv zs  = failCE "tuple9" "expected 9 values" zs
instance (Conv a1,Conv a2,Conv a3,Conv a4,Conv a5,Conv a6,Conv a7,Conv a8,Conv a9,Conv a10) => Conv (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) where
  conv [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10] = (,,,,,,,,,) <$> conv [a1] <*> conv [a2] <*> conv [a3] <*> conv [a4] <*> conv [a5] <*> conv [a6] <*> conv [a7] <*> conv [a8] <*> conv [a9] <*> conv [a10]
  conv zs  = failCE "tuple10" "expected 10 values" zs
instance (Conv a1,Conv a2,Conv a3,Conv a4,Conv a5,Conv a6,Conv a7,Conv a8,Conv a9,Conv a10,Conv a11) => Conv (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) where
  conv [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11] = (,,,,,,,,,,) <$> conv [a1] <*> conv [a2] <*> conv [a3] <*> conv [a4] <*> conv [a5] <*> conv [a6] <*> conv [a7] <*> conv [a8] <*> conv [a9] <*> conv [a10] <*> conv [a11]
  conv zs  = failCE "tuple11" "expected 11 values" zs
instance (Conv a1,Conv a2,Conv a3,Conv a4,Conv a5,Conv a6,Conv a7,Conv a8,Conv a9,Conv a10,Conv a11,Conv a12) => Conv (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12) where
  conv [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12] = (,,,,,,,,,,,) <$> conv [a1] <*> conv [a2] <*> conv [a3] <*> conv [a4] <*> conv [a5] <*> conv [a6] <*> conv [a7] <*> conv [a8] <*> conv [a9] <*> conv [a10] <*> conv [a11] <*> conv [a12]
  conv zs  = failCE "tuple12" "expected 12 values" zs
instance (Conv a1,Conv a2,Conv a3,Conv a4,Conv a5,Conv a6,Conv a7,Conv a8,Conv a9,Conv a10,Conv a11,Conv a12,Conv a13) => Conv (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13) where
  conv [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13] = (,,,,,,,,,,,,) <$> conv [a1] <*> conv [a2] <*> conv [a3] <*> conv [a4] <*> conv [a5] <*> conv [a6] <*> conv [a7] <*> conv [a8] <*> conv [a9] <*> conv [a10] <*> conv [a11] <*> conv [a12] <*> conv [a13]
  conv zs  = failCE "tuple13" "expected 13 values" zs
instance (Conv a1,Conv a2,Conv a3,Conv a4,Conv a5,Conv a6,Conv a7,Conv a8,Conv a9,Conv a10,Conv a11,Conv a12,Conv a13,Conv a14) => Conv (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14) where
  conv [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14] = (,,,,,,,,,,,,,) <$> conv [a1] <*> conv [a2] <*> conv [a3] <*> conv [a4] <*> conv [a5] <*> conv [a6] <*> conv [a7] <*> conv [a8] <*> conv [a9] <*> conv [a10] <*> conv [a11] <*> conv [a12] <*> conv [a13] <*> conv [a14]
  conv zs  = failCE "tuple14" "expected 14 values" zs
instance (Conv a1,Conv a2,Conv a3,Conv a4,Conv a5,Conv a6,Conv a7,Conv a8,Conv a9,Conv a10,Conv a11,Conv a12,Conv a13,Conv a14,Conv a15) => Conv (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15) where
  conv [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15] = (,,,,,,,,,,,,,,) <$> conv [a1] <*> conv [a2] <*> conv [a3] <*> conv [a4] <*> conv [a5] <*> conv [a6] <*> conv [a7] <*> conv [a8] <*> conv [a9] <*> conv [a10] <*> conv [a11] <*> conv [a12] <*> conv [a13] <*> conv [a14] <*> conv [a15]
  conv zs  = failCE "tuple15" "expected 15 values" zs
instance (Conv a1,Conv a2,Conv a3,Conv a4,Conv a5,Conv a6,Conv a7,Conv a8,Conv a9,Conv a10,Conv a11,Conv a12,Conv a13,Conv a14,Conv a15,Conv a16) => Conv (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16) where
  conv [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16] = (,,,,,,,,,,,,,,,) <$> conv [a1] <*> conv [a2] <*> conv [a3] <*> conv [a4] <*> conv [a5] <*> conv [a6] <*> conv [a7] <*> conv [a8] <*> conv [a9] <*> conv [a10] <*> conv [a11] <*> conv [a12] <*> conv [a13] <*> conv [a14] <*> conv [a15] <*> conv [a16]
  conv zs  = failCE "tuple16" "expected 16 values" zs

convfloat :: forall a. (Show a, Fractional a, Read a) => SqlValue -> Either EE a
convfloat = go
  where
  go z@(SqlByteString bs) =
       let w = B.unpack $ trimBS bs
           w1 = if "." `isPrefixOf` w then '0':w else w
       in case reads w1 of -- oracle driver is lacking support for numeric and integer etc [real and number are ok]
           [(a :: a,"")] -> return a
           o@[(_a :: a, zs)] | all (=='\0') zs -> failCE "Num" ("convfloat: mssql?: invalid float from sqlbytestring has nulls cos truncated? (append with 'e' or cast explicitly) bs=[" ++ B.unpack bs ++ "] reads output=" ++ show o) [z]
           o -> failCE "Float" ("convfloat: for oracle?: invalid number from sqlbytestring reads output=" ++ show o ++ " bs=" ++ show bs) [z]
  go (SqlDouble d) = return $ realToFrac d
  go (SqlInteger i) = return $ fromIntegral i
  go (SqlInt32 i) = return $ fromIntegral i
  go (SqlInt64 i) = return $ fromIntegral i
  go o = failCE "Float" "convfloat:invalid float number" [o]

convnum :: forall a. (Show a, Num a, Read a) => SqlValue -> Either EE a
convnum = go
  where
  go (SqlChar i) = return $ fromIntegral $ ord i
  go (SqlInt32 i) = return $ fromIntegral i
  go (SqlInt64 i) = return $ fromIntegral i
  go (SqlInteger i) = return $ fromIntegral i
  go z@(SqlByteString bs) =
       case reads (B.unpack $ trimBS bs) of -- oracle driver is lacking support for numeric and integer etc [real and number are ok]
         [(a :: a,"")] -> return a
--         [(a :: a, zs)] | all (=='\0') zs -> failCE "Num" ("convnum: mssql?: invalid number from sqlbytestring has nulls cos truncated? (append with 'e' or cast explicitly) bs=[" ++ B.unpack bs ++ "] reads output=" ++ show o) [z]
         o -> failCE "Num" ("convnum: for oracle?: invalid number from sqlbytestring bs=[" ++ B.unpack bs ++ "] reads output=" ++ show o) [z]
  go z@(SqlDouble d) =
      let r = d - fromIntegral (floor d :: Integer) -- yep
      in if r == 0 then return $ fromIntegral (truncate d :: Integer)
         else failCE "Num" "convnum:invalid integer even for oracle as it has a fraction" [z]
  go o = failCE "Num" ("convnum:invalid number " ++ showSqlTypeHead o) [o]

convbool :: SqlValue -> Either EE Bool
convbool = go
  where
  go (SqlBool b) = return b
  go (SqlChar '\0') = return False
  go (SqlChar '\1') = return True
  go z@(SqlChar _) = failCE "Bool" "convbool: invalid bool: found SqlChar but not 0/1" [z]
  go a = convnum @Int a >>= \case
                             1 -> return True
                             0 -> return False
                             _ -> failCE "Bool" ("convbool: invalid bool as a number: expected 0/1 but found " ++ showSqlTypeHead a) [a]

showSqlTypeHead :: SqlValue -> String
showSqlTypeHead = \case
   SqlString {} -> "SqlString"
   SqlByteString {} -> "SqlByteString"
   SqlWord32 {} -> "SqlWord32"
   SqlWord64 {} -> "SqlWord64"
   SqlInt32 {} -> "SqlInt32"
   SqlInt64 {} -> "SqlInt64"
   SqlInteger {} -> "SqlInteger"
   SqlChar {} -> "SqlChar"
   SqlBool {} -> "SqlBool"
   SqlDouble {} -> "SqlDouble"
   SqlRational {} -> "SqlRational"
   SqlLocalDate {} -> "SqlLocalDate"
   SqlLocalTimeOfDay {} -> "SqlLocalTimeOfDay"
   SqlZonedLocalTimeOfDay {} -> "SqlZonedLocalTimeOfDay"
   SqlLocalTime {} -> "SqlLocalTime"
   SqlZonedTime {} -> "SqlZonedTime"
   SqlUTCTime {} -> "SqlUTCTime"
   SqlDiffTime {} -> "SqlDiffTime"
   SqlPOSIXTime {} -> "SqlPOSIXTime"
   SqlNull {} -> "SqlNull"

trimBS :: ByteString -> ByteString
trimBS = f . f
   where f = B.reverse . B.dropWhile isSpace
