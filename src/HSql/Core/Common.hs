{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

{- |
Module      : HSql.Core.Common
Description : miscellaneous functions
Copyright   : (c) Grant Weyburne, 2021
License     : BSD-3
-}
module HSql.Core.Common where

import Control.Lens
import Data.Bool
import Data.List.NonEmpty (NonEmpty (..))
import Data.Pos
import Data.Semigroup.Foldable (intercalate1)
import Data.Text (Text)
import qualified Data.Vinyl.Functor as V
import Database.HDBC (SqlColDesc (..), SqlTypeId (..), SqlValue (..))
import Primus.NonEmpty

-- | pattern synonym for vinyl's identity type
{-# COMPLETE I #-}

pattern I :: a -> V.Identity a
pattern I a = V.Identity a

-- | list of column types representing metadata for a row of data
type RMeta = [SqlColDesc]

-- | sql result set type
type ResultSet = Either Int (RMeta, [[SqlValue]])

-- | metadata for a single string-like column
metaDefString :: Int -> String -> SqlColDesc
metaDefString i nm = SqlColDesc nm SqlVarCharT (Just i) Nothing Nothing Nothing

-- | metadata for a single integer-like column
metaDefInteger :: String -> SqlColDesc
metaDefInteger nm = SqlColDesc nm SqlIntegerT (Just 10) Nothing Nothing Nothing

-- | metadata for a single bool-like column
metaDefBool :: String -> SqlColDesc
metaDefBool nm = SqlColDesc nm SqlTinyIntT (Just 1) Nothing Nothing Nothing

-- | pretty print meta data
showMeta :: Bool -> SqlColDesc -> String
showMeta verbose cd =
  let a1 = case colType cd of
        SqlUnknownT s -> "[Unknown:" <> s <> "]"
        x -> drop 3 (show x) ^. _init
      a2 = maybe mempty show (colSize cd)
      a3 = maybe mempty show (colOctetLength cd)
      a4 = maybe mempty show (colDecDigits cd)
      a5 :: String
      a5 = maybe mempty (bool mempty " not null") (colNullable cd)
      a34 = case (a3, a4) of
        ([], []) -> mempty
        (_, _) -> "," <> a3 <> "," <> a4
   in a1 <> "(" <> a2 <> a34 <> ")" <> bool mempty a5 verbose

-- | generates n sql input placeholders (used for sql inserts)
qqsn :: Pos -> Text
qqsn = vvs . flip replicate1 "?"

-- | generates length n sql input placeholders (used for sql inserts) -- can be zero length if no insertable fields eg identity only!
qqs :: NonEmpty a -> Text
qqs = qqsn . lengthP

-- | concatenates column names together used for sql inserts
vvs :: NonEmpty Text -> Text
vvs xs = "(" <> intercalate1 "," xs <> ")"

-- | generates m by n sql input placeholders (used for sql inserts for multiple rows at a time)
qqrc :: (Pos, Pos) -> Text
qqrc (r, c) = intercalate1 ", " (replicate1 r (qqsn c))
