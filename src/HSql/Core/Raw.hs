{-# OPTIONS -Wall #-}
{-# OPTIONS -Wcompat #-}
{-# OPTIONS -Wincomplete-record-updates #-}
{-# OPTIONS -Wincomplete-uni-patterns #-}
{-# OPTIONS -Wno-redundant-constraints #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- |
Module      : HSql.Core.Raw
Description : holds all the sql values untranlated in a list
Copyright   : (c) Grant Weyburne, 2016
License     : BSD-3
Maintainer  : gbwey9@gmail.com
-}
module HSql.Core.Raw where
import Database.HDBC (SqlValue)
import qualified GHC.Generics as G
import Generics.SOP
import Control.DeepSeq (NFData)

newtype Raw = Raw { unRaw :: [SqlValue] } deriving (Show,G.Generic,NFData)
instance Generic Raw
instance HasDatatypeInfo Raw
