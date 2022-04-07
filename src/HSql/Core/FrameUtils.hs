{-# OPTIONS -Wno-missing-local-signatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      : HSql.Core.VinylUtils
Description : utilities for holds a singleton value 'HSql.Core.One.One'
Copyright   : (c) Grant Weyburne, 2016
License     : BSD-3

handles a tuple of size one
-}
module HSql.Core.FrameUtils where

import qualified Control.Foldl as FL (Fold (..))
import qualified Control.Monad.State.Strict as S (state)
import qualified Control.Scanl as FS (Scan (..))
import Data.Foldable
import Data.Vinyl
import qualified Data.Vinyl.Functor as V
import Frames ((:->))
import qualified Frames as F (stripNames)
import GHC.TypeLits (KnownSymbol)

-- | shorthand for a record of labelled fields
type F = Rec ElField -- :: [(Symbol, Type)] -> Type

-- works for any Foldable (F rs) so works for Frame or []

-- | convert a container of frames to a list of vinyl records by stripping out the names
frameToList :: forall rs t. (StripFieldNames rs, Foldable t) => t (F rs) -> [Rec V.Identity (Unlabeled rs)]
frameToList = foldMap' (pure . F.stripNames)

{- | create a post scan using a fold for a vinyl record and stash it in a new field called "s1"
   eg create a column with a running average
-}
postscanF ::
  forall s1 b us.
  (KnownSymbol s1) =>
  FL.Fold (Rec V.ElField us) b ->
  FS.Scan (Rec V.ElField us) (Rec V.ElField ((s1 :-> b) ': us))
postscanF (FL.Fold step begin done) = FS.Scan (S.state . step') begin
 where
  step' a x = (V.Field b :& a, x')
   where
    x' = step x a
    b = done x'

-- | create a pre scan using a fold for a vinyl record and stash it in a new field called "s1"
prescanF ::
  forall s1 b us.
  (KnownSymbol s1) =>
  FL.Fold (Rec V.ElField us) b ->
  FS.Scan (Rec V.ElField us) (Rec V.ElField ((s1 :-> b) ': us))
prescanF (FL.Fold step begin done) = FS.Scan (S.state . step') begin
 where
  step' a x = (V.Field b :& a, x')
   where
    x' = step x a
    b = done x
