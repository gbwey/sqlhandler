{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoStarIsType #-}
{- |
Module      : ErrorHandler
Description : This module provides methods for creating and displaying errors in the large sum type 'SE'
Copyright   : (c) Grant Weyburne, 2016
License     : BSD-3
Maintainer  : gbwey9@gmail.com

-}
module HSql.Core.ErrorHandler where
import Data.Vinyl
import qualified Data.Vinyl.CoRec as VC
import Data.Vinyl.CoRec (CoRec(..),weakenCoRec)
import qualified Data.Vinyl.Functor as V
import Data.Vinyl.TypeLevel hiding (Nat)
import Control.Lens hiding (rmap,Identity,Const,op)
import Data.Maybe
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as N
import Data.List (intercalate)
import GHC.Generics (Generic)
import Data.Typeable

import Database.HDBC (SqlValue)
import HSql.Core.Common
import Control.DeepSeq (NFData)

type EE = NonEmpty ConvE

-- | convenience method used for signaling a conversion error see 'HSql.Core.Conv'
failCE :: String -> String -> [SqlValue] -> Either EE a
failCE a b c = Left (ConvE a b c :| [])

data ConvE = ConvE { _cvType :: !String, _cvMessage :: !String, _cvSqlValue :: ![SqlValue] } deriving (Generic, Eq)
instance NFData ConvE
-- makeLenses ''ConvE

instance Show ConvE where
  show (ConvE a b c) = "ConvE type=" ++ a ++ " msg=" ++ b ++ " sqlvalues=" ++ intercalate "," (map show c)


-- | convenience method used for signaling a decoding error see 'HSql.Core.Decoder'
type DE' = '[ConvE, DecodingE]
type DE = NonEmpty (CoRec V.Identity DE')

liftCE :: NonEmpty ConvE -> DE
liftCE = fmap (CoRec . V.Identity)

failDE :: String -> String -> [SqlValue] -> Either DE a
failDE a b c = Left (CoRec (V.Identity (DecodingE a b c)) :| [])

data DecodingE = DecodingE { _deMethod :: !String, _deMessage :: !String, _deSqlValues :: ![SqlValue] } deriving (Eq, Generic)
instance NFData DecodingE

-- makeLenses ''DecodingE

instance Show DecodingE where
  show (DecodingE a b c) =
    "DecodingE method=" ++ a
    ++ " | " ++ b ++ " | "
    ++ " sqlvalues=" ++ intercalate "," (map show c)

-- | predicate failure when retrieving an update resultset
data UpdNE = UpdNE { _unMethod :: !String, _unPos :: !Int, _unMessage :: !String } deriving (Generic, Show, Eq)
instance NFData UpdNE

-- | unconsumed resultset ie there were more resultsets returned than the type signature specifies
data UnconsumedColE = UnconsumedColE { _uccMethod :: !String, _uccPos :: !Int, _uccMessage :: !String, _uccRest :: ![ResultSet] } deriving (Generic, Show, Eq)
instance NFData UnconsumedColE

-- | expected exactly one row for SelOne
data SingleColE = SingleColE { _sicInstance :: !String, _sicPos :: !(Maybe Int), _sicMessage :: !String, _sicRss :: ![ResultSet] } deriving (Generic, Show, Eq)
-- | invalid resultset type: ie returned an update type vs a select type
instance NFData SingleColE

data UnexpectedResultSetTypeE = UnexpectedResultSetTypeE { _urstMethod :: !String, _urstMessage :: !String, _urstRss :: !ResultSet } deriving (Generic, Show, Eq)
instance NFData UnexpectedResultSetTypeE

-- | no more resultsets were returned but the type definition expected one
data NoResultSetE = NoResultSetE { _nrMethod :: !String, _nrPos :: !Int, _nrMessage :: !String } deriving (Generic, Show, Eq)
instance NFData NoResultSetE

-- | programmer error that shouldn't happen!
data BadE = BadE { _badMethod :: !String, _badMessage :: !String, _badData :: !String } deriving (Generic, Show, Eq)
instance NFData BadE

-- | 'SE'' contains a promoted list of all possible errors
type SE' = '[ UpdNE
            , UnconsumedColE
            , SingleColE
            , UnexpectedResultSetTypeE
            , NoResultSetE
            , BadE
            , ConvE
            , DecodingE]

type SE = NonEmpty (CoRec V.Identity SE')

seShortMessages' :: Either SE x -> [String]
seShortMessages' x = x ^.. _Left . traverse . to seShortMessage

seShortMessages :: SE -> [String]
seShortMessages = map seShortMessage . N.toList

-- | pulls out a shorter message from errors
seShortMessage :: CoRec V.Identity SE' -> String
seShortMessage x =
  let f :: Typeable a => a -> String
      f e = (tyConName . typeRepTyCon . typeOf) e ++ ": "
  in VC.match x $
       VC.H (\e -> f e ++ _unMethod e ++ " " ++ _unMessage e)
    :& VC.H (\e -> f e ++ _uccMethod e ++ " " ++ _uccMessage e)
    :& VC.H (\e -> f e ++ _sicInstance e ++ " " ++ _sicMessage e)
    :& VC.H (\e -> f e ++ _urstMethod e ++ " " ++ _urstMessage e)
    :& VC.H (\e -> f e ++ _nrMethod e ++ " " ++ _nrMessage e)
    :& VC.H (\e -> f e ++ _badMethod e ++ " " ++ _badMessage e)
    :& VC.H (\e -> f e ++ _cvType e ++ " " ++ _cvMessage e)
    :& VC.H (\e -> f e ++ _deMethod e ++ " " ++ _deMessage e)
    :& RNil

-- | lift a decoding error to the larger SE error
liftDE :: DE -> SE
liftDE = fmap (weakenCoRec
             . weakenCoRec
             . weakenCoRec
             . weakenCoRec
             . weakenCoRec
             . weakenCoRec)

failNE :: String -> Int -> String -> Either SE a
failNE a b c = failSE (UpdNE a b c)

failUCC :: String -> Int -> String -> [ResultSet] -> Either SE a
failUCC a b c d = failSE (UnconsumedColE a b c d)

failURST :: String -> String -> ResultSet -> Either SE a
failURST a b c = failSE (UnexpectedResultSetTypeE a b c)

failSIC :: String -> Maybe Int -> String -> [ResultSet] -> Either SE a
failSIC a b c d = failSE (SingleColE a b c d)

failNR :: String -> Int -> String -> Either SE a
failNR a b c = failSE (NoResultSetE a b c)

failBad :: String -> String -> String -> Either SE a
failBad a b c = failSE (BadE a b c)

failSE :: RElem x SE' (RIndex x SE') => x -> Either SE a
failSE x = Left (CoRec (V.Identity x) :| [])

showSE :: SE -> String
showSE = intercalate "\n" . map show . N.toList

-- eg hasError @SingleColE
-- | 'hasError' checks for errors of a given type p (use typeapplications)
hasError :: forall p a . NatToInt (RIndex p SE') => Either SE a -> Bool
hasError = not . null . getErrors @p

-- | 'getErrors' returns list of errors of given type p (use typeapplications)
getErrors :: forall p a . NatToInt (RIndex p SE') => Either SE a -> [p]
getErrors = either (xes @p) (const [])

-- eg: left (xes @UpdNE) a
-- | 'xes' pulls out all the messages of a type t from a nonempty list of vinyl corecords
xes :: forall t ts . NatToInt (RIndex t ts) => NonEmpty (CoRec V.Identity ts) -> [t]
xes = mapMaybe VC.asA . N.toList

xes' :: forall t ts . NatToInt (RIndex t ts) => NonEmpty (CoRec V.Identity ts) -> Bool
xes' = not . null . xes @t

xes'' :: forall t ts a . NatToInt (RIndex t ts) => Either (NonEmpty (CoRec V.Identity ts)) a -> Bool
xes'' (Left v) = xes' @t v
xes'' (Right _) = False




