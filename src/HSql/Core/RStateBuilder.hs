{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}

{- |
 Module      : HSql.Core.RStateBuilder
 Description : builders for creating RState
 Copyright   : (c) Grant Weyburne, 2021
 License     : BSD-3
-}
module HSql.Core.RStateBuilder where

import Control.Arrow
import Data.Functor.Identity
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as N
import Data.Semigroup
import Data.Vinyl
import DocUtils.Doc
import HSql.Core.Decoder
import HSql.Core.Encoder
import HSql.Core.Sql
import Utils.Error

-- | modify RState contents within a vinyl record at the first position
mapRState1 :: DefDec (SingleIn c) => (a -> c) -> Rec RState (a : rs) -> Rec RState (c : rs)
mapRState1 f = rstateChange1 (mapRState f)

-- | combinator for modifying RState contents
mapRState :: DefDec (SingleIn b) => (a -> b) -> RState a -> RState b
mapRState f = RState defDec . f . rsOut

-- | combinator for modify RState contents at position 1 for any Functor
rstateChange1' :: Functor f => (RState a -> f (RState a')) -> Rec RState (a ': rs) -> f (Rec RState (a' ': rs))
rstateChange1' f (r :& rs) = (:& rs) <$> f r

-- | combinator for modify RState contents at position 2 for any Functor
rstateChange2' :: Functor f => (RState b -> f (RState b')) -> Rec RState (a ': b ': rs) -> f (Rec RState (a ': b' ': rs))
rstateChange2' f (a :& r :& rs) = (\x -> a :& x :& rs) <$> f r

-- | combinator for modify RState contents at position 1 for Identity
rstateChange1 :: (RState b -> RState c) -> Rec RState (b ': rs) -> Rec RState (c ': rs)
rstateChange1 f = runIdentity . rstateChange1' (Identity <$> f)

-- | combinator for modify RState contents at position 2 for Identity
rstateChange2 :: (RState b -> RState b') -> Rec RState (a ': b ': rs) -> Rec RState (a ': b' ': rs)
rstateChange2 f = runIdentity . rstateChange2' (Identity <$> f)

-- | combinator for concatenating a non empty list of vinyl records into one
recRStateSelAnyConcat :: Semigroup (RState a) => NonEmpty (Rec RState '[a]) -> Rec RState '[a]
recRStateSelAnyConcat rs =
  let ns = N.map (\(r :& _) -> r) rs
   in sconcat ns :& RNil

-- | flatten 'Some' 'Sel' into a single 'Sel'
flattenSomeSel :: RState (Some (Sel a)) -> RState (Sel a)
flattenSomeSel
  RState
    { rsIn = SomeP (SelP dec)
    , rsOut = Some (Sel x meta :| xs)
    } =
    RState
      { rsIn = SelP dec
      , rsOut = Sel (x <> L.concatMap (\(Sel as _) -> as) xs) meta
      }

-- | convert an untyped select to a typed one
selRawToSel :: (Show a, DefDec (Dec a)) => RState SelRaw -> Either String (RState (Sel a))
selRawToSel
  RState
    { rsIn = SelRawP
    , rsOut = SelRaw xs meta
    } = lmsg "selRawToSel" $ do
    as' <- left psiS $ mapM (unDec defDec) xs
    as <- case L.partition (null . snd) as' of
      (z, []) -> return $ map fst z
      z@(_, _ : _) -> Left $ "found leftovers " ++ psiS z
    return
      RState
        { rsIn = SelP defDec
        , rsOut = Sel as meta
        }

-- | convert a 'Sel' to 'SelRaw'
selToSelRaw :: DefEnc (Enc a) => RState (Sel a) -> RState SelRaw
selToSelRaw
  RState
    { rsIn = SelP _
    , rsOut = Sel xs meta
    } =
    RState
      { rsIn = SelRawP
      , rsOut = SelRaw (map (unEnc defEnc) xs) meta
      }

-- | try to convert a 'Sel' to 'SelRow'
selToSelRow :: RState (Sel a) -> Either String (RState (SelRow a))
selToSelRow
  RState
    { rsIn = SelP dec
    , rsOut = Sel xs meta
    } = lmsg "selToSelRow" $ do
    as <- case xs of
      [x] -> return x
      o -> Left $ "must have exactly one row to convert to SelRow but found " ++ show (length o)
    return
      RState
        { rsIn = SelRowP dec
        , rsOut = SelRow as meta
        }

-- | try to convert a 'SelCol' to 'SelRowCol'
selColToSelRowCol :: RState (SelCol a) -> Either String (RState (SelRowCol a))
selColToSelRowCol
  RState
    { rsIn = SelColP dec
    , rsOut = SelCol xs meta
    } = lmsg "selColToSelRowCol" $ do
    as <- case xs of
      [x] -> return x
      o -> Left $ "must have exactly one row to convert to SelRowCol but found " ++ show (length o)
    return
      RState
        { rsIn = SelRowColP dec
        , rsOut = SelRowCol as meta
        }

-- | flatten 'Some' 'SelRow' into a single 'Sel'
flattenSomeSelRow :: RState (Some (SelRow a)) -> RState (Sel a)
flattenSomeSelRow
  RState
    { rsIn = SomeP (SelRowP dec)
    , rsOut = Some (SelRow x meta :| xs)
    } =
    RState
      { rsIn = SelP dec
      , rsOut = Sel (x : map (\(SelRow a _) -> a) xs) meta
      }

-- | try to flatten 'Alle' 'Sel' into a single 'Sel'
flattenAlleSel :: RState (Alle (Sel a)) -> Either String (RState (Sel a))
flattenAlleSel
  RState
    { rsIn = AlleP (SelP dec)
    , rsOut = Alle xss
    } = case xss of
    [] -> Left "flattenAlleSel: need at least one resultset otherwise we have no metadata to create a valid Sel"
    Sel x meta : xs ->
      return
        RState
          { rsIn = SelP dec
          , rsOut = Sel (x <> L.concatMap (\(Sel as _) -> as) xs) meta
          }

-- | try to flatten 'Alle' 'SelCol' into a single 'SelCol'
flattenAlleSelCol :: RState (Alle (SelCol a)) -> Either String (RState (SelCol a))
flattenAlleSelCol
  RState
    { rsIn = AlleP (SelColP dec)
    , rsOut = Alle xss
    } = case xss of
    [] -> Left "flattenAlleSelCpl: need at least one resultset otherwise we have no metadata to create a valid SelCol"
    SelCol x meta : xs ->
      return
        RState
          { rsIn = SelColP dec
          , rsOut = SelCol (x <> L.concatMap (\(SelCol as _) -> as) xs) meta
          }
