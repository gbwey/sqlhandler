{-# OPTIONS -Wno-orphans #-}

{- |
Module      : HSql.Core.One
Description : holds a singleton value 'One'
Copyright   : (c) Grant Weyburne, 2016
License     : BSD-3
-}
module HSql.Core.One (
  module Primus.One,
) where

import Generics.SOP
import Primus.One

instance Generic (One a)
instance HasDatatypeInfo (One a)
