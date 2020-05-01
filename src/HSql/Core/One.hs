{-# OPTIONS -Wall #-}
{-# OPTIONS -Wcompat #-}
{-# OPTIONS -Wincomplete-record-updates #-}
{-# OPTIONS -Wincomplete-uni-patterns #-}
{-# OPTIONS -Wno-redundant-constraints #-}
{-# LANGUAGE DeriveGeneric #-}
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

-- | One holds a single value. To use wprint we need a SOP Generics instance
newtype One a = One { unOne :: a } deriving (G.Generic, Show)
instance GS.Generic (One a)
instance GS.HasDatatypeInfo (One a)
