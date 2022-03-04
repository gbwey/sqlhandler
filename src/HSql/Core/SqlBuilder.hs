{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}

{- |
 Module      : HSql.Core.SqlBuilder
 Description : builders for creating RState so we can test the 'HSql.Core.Sql' dsl
 Copyright   : (c) Grant Weyburne, 2021
 License     : BSD-3
-}
module HSql.Core.SqlBuilder where

import Data.Bool
import Data.List.NonEmpty (NonEmpty (..))
import Data.Vinyl
import Database.HDBC (SqlValue (..))
import GHC.TypeLits (KnownNat)
import HSql.Core.Common
import HSql.Core.Decoder
import HSql.Core.Operator
import HSql.Core.Sql
import Utils.Error
import Utils.One
import qualified Utils.TypeLevel as TP (pnat)

-- | convenience method to create rows in a vinyl record so we can use wprint directly with it
mkSelRecords :: DefDec (Dec a) => [String] -> [a] -> Rec RState '[Sel a]
mkSelRecords meta xs = mkRState (mkSel meta xs) :& RNil

-- | create a 'SelRowCol'
mkSelRowCol :: [String] -> a -> SelRowCol a
mkSelRowCol cols val =
  SelRowCol val (metaDefString 30 <$> cols)

-- | create a 'SelCol'
mkSelCol :: [String] -> [a] -> SelCol a
mkSelCol cols vals =
  SelCol vals (metaDefString 30 <$> cols)

-- | create a 'Sel'
mkSel :: [String] -> [a] -> Sel a
mkSel cols vals =
  -- dont know the length so up to you
  Sel vals (metaDefString 30 <$> cols)

-- | create a 'SelRow' 'One' so we can display it
mkSelRowOne :: [String] -> a -> SelRow (One a)
mkSelRowOne cols = mkSelRow cols . One

-- | create a 'Sel' 'One' so we can display it
mkSelOne :: [String] -> [a] -> Sel (One a)
mkSelOne cols = mkSel cols . map One

-- | create a 'SelRow'
mkSelRow :: [String] -> a -> SelRow a
mkSelRow cols val =
  -- dont know the length so up to you
  SelRow val (metaDefString 30 <$> cols)

-- | create a 'Upd'
mkUpd :: Int -> Upd
mkUpd = Upd

-- | create a 'UpdN'
mkUpdN :: forall (op :: Op). Int -> UpdN op
mkUpdN = UpdN

-- | create a 'Both'
mkBoth, (|*|) :: lhs -> rhs -> lhs :*: rhs
mkBoth = Both
(|*|) = mkBoth

infixl 3 |*|

-- | create a 'Or'
mkOr :: Either lhs rhs -> lhs :+: rhs
mkOr =
  \case
    Left l1 -> Or (Left l1)
    Right r1 -> Or (Right r1)

-- | create a 'Or' and choose one side based on a boolean
mkOr' :: Bool -> lhs -> rhs -> lhs :+: rhs
mkOr' b l1 r1 =
  bool (Or (Left l1)) (Or (Right r1)) b

-- | create a left 'Or' using an operator
(|<|) :: lhs -> rhs -> lhs :+: rhs
(|<|) = mkOr' False

infixl 2 |<|

-- | create a right 'Or' using an operator
(|>|) :: lhs -> rhs -> lhs :+: rhs
(|>|) = mkOr' True

infixl 2 |>|

-- | create a 'May'
mkMaybe :: Maybe a -> May a
mkMaybe =
  \case
    Nothing -> May Nothing
    Just a -> May (Just a)

-- | create a 'SelRaw'
mkSelRaw :: [String] -> [[SqlValue]] -> SelRaw
mkSelRaw metas vals = SelRaw vals (metaDefString 30 <$> metas)

-- | create a 'Exact'
mkExact ::
  forall n a.
  KnownNat n =>
  NonEmpty a ->
  Exact n a
mkExact ns =
  let n = TP.pnat @n
   in if length ns /= n
        then normalError $ "mkExact: mismatched lengths ns=" ++ show (length ns) ++ " but expected n=" ++ show n
        else Exact ns

-- | create a 'Range'
mkRange ::
  forall m n a.
  (KnownNat m, KnownNat n) =>
  [a] ->
  Range m n a
mkRange xs =
  let m = TP.pnat @m
      n = TP.pnat @n
      len = length xs
   in if len < m || len > n
        then normalError $ "mkRange: mismatched lengths xs=" ++ show len ++ " but must be between " ++ show (m, n)
        else Range xs

-- | create a 'Alle'
mkAlle :: [a] -> Alle a
mkAlle = Alle

-- | create a 'Some'
mkSome :: NonEmpty a -> Some a
mkSome = Some

-- | create a 'Rev'
mkRev :: a -> Rev a
mkRev = Rev

-- | create a 'RState'
mkRState :: DefDec (SingleIn a) => a -> RState a
mkRState = RState defDec
