{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module      : HSql.Core.Raw
Description : holds all the sql values untranlated in a list
Copyright   : (c) Grant Weyburne, 2016
License     : BSD-3
-}
module HSql.Core.Raw where

import Control.DeepSeq (NFData)
import Database.HDBC (SqlValue)
import qualified GHC.Generics as G
import Generics.SOP

-- | holds the raw 'SqlValue's
newtype Raw = Raw {unRaw :: [SqlValue]}
  deriving stock (G.Generic, Show, Eq)
  deriving newtype (NFData)

instance Generic Raw
instance HasDatatypeInfo Raw
