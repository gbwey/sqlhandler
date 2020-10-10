{-# OPTIONS -Wall #-}
{-# LANGUAGE PatternSynonyms #-}
module HSql.Core.Common where
import Database.HDBC (SqlValue, SqlColDesc)
import qualified Data.Vinyl.Functor as V

{-# COMPLETE I #-}
pattern I :: a -> V.Identity a
pattern I a = V.Identity a

type RMeta = [SqlColDesc]
type ResultSet = Either Int (RMeta,[[SqlValue]])
