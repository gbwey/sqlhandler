{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoStarIsType #-}
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

newtype Raw = Raw { unRaw :: [SqlValue] }
  deriving stock (G.Generic, Show, Eq)
  deriving newtype (NFData)
instance Generic Raw
instance HasDatatypeInfo Raw
