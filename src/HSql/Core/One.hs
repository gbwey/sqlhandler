{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoStarIsType #-}
{- |
Module      : HSql.Core.One
Description : holds a singleton value 'One'
Copyright   : (c) Grant Weyburne, 2016
License     : BSD-3
Maintainer  : gbwey9@gmail.com

handles a tuple of size one. this is a special type that distinguishes a singleton value from a ntuple
-}
module HSql.Core.One where
import qualified GHC.Generics as G
import qualified Generics.SOP as GS
import Control.DeepSeq (NFData)

-- | One holds a single value. To use wprint we need a SOP Generics instance
newtype One a = One { unOne :: a }
  deriving stock (G.Generic, Show)
  deriving newtype (NFData)

instance GS.Generic (One a)
instance GS.HasDatatypeInfo (One a)
