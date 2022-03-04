{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : ErrorHandler
Description : This module provides methods for creating and displaying errors in the large sum type 'SE'
Copyright   : (c) Grant Weyburne, 2016
License     : BSD-3
-}
module HSql.Core.ErrorHandler where

import Control.DeepSeq (NFData)
import Control.Lens hiding (Const, Identity, op, rmap)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as N
import Data.Maybe
import Data.Typeable
import Data.Vinyl hiding ((<+>))
import Data.Vinyl.CoRec (CoRec (..), weakenCoRec)
import qualified Data.Vinyl.CoRec as VC
import qualified Data.Vinyl.Functor as V
import Data.Vinyl.TypeLevel hiding (Nat)
import Database.HDBC (SqlValue)
import GHC.Generics (Generic)
import HSql.Core.Common

-- | type of conversion errors
type EE = NonEmpty ConvE

-- | convenience method used for signaling a conversion error see 'HSql.Core.Conv'
failCE :: String -> String -> [SqlValue] -> Either EE a
failCE a b c = Left (pure $ ConvE a b c)

-- | conversion error record
data ConvE = ConvE
  { cvType :: !String
  -- ^ type of data
  , cvMessage :: !String
  -- ^ error message
  , cvSqlValue :: ![SqlValue]
  -- ^ fields in error
  }
  deriving stock (Show, Generic, Eq)

instance NFData ConvE

-- | convenience method used for signaling a decoding error see 'HSql.Core.Decoder'
type DE' = '[ConvE, DecodingE]

-- | type of decoding and conversion errors
type DE = NonEmpty (CoRec V.Identity DE')

-- | lift a conversion error to 'DE'
liftCE :: NonEmpty ConvE -> DE
liftCE = fmap (CoRec . V.Identity)

-- | convenience method used for signaling a conversion error or decoding error
failDE :: String -> String -> String -> [SqlValue] -> Either DE a
failDE a b c d = Left (pure $ CoRec (V.Identity (DecodingE a b c d)))

-- | decoding error record
data DecodingE = DecodingE
  { deMethod :: !String
  , deMessage :: !String
  , deMessageDetailed :: !String
  , deSqlValues :: ![SqlValue]
  }
  deriving stock (Show, Eq, Generic)

instance NFData DecodingE

-- | predicate failure when retrieving an update resultset
data UpdNE = UpdNE
  { unMethod :: !String
  , unPos :: !Int
  , unMessage :: !String
  }
  deriving stock (Generic, Show, Eq)

instance NFData UpdNE

-- | unconsumed resultset ie there were more resultsets returned than the type signature specifies
data UnconsumedResultSetE = UnconsumedResultSetE
  { ucrsMethod :: !String
  , ucrsPos :: !Int
  , ucrsMessage :: !String
  , ucrsRest :: ![ResultSet]
  }
  deriving stock (Generic, Show, Eq)

instance NFData UnconsumedResultSetE

-- | expect exactly one column for SelCol SelRowCol
data MoreThanOneColumnE = MoreThanOneColumnE
  { mtocMethod :: !String
  , mtocPos :: !Int
  , mtocMessage :: !String
  , mtocRest :: !ResultSet
  }
  deriving stock (Generic, Show, Eq)

instance NFData MoreThanOneColumnE

-- | unconsumed columns: decoding errors or extra columns?
data UnconsumedColumnE = UnconsumedColumnE
  { uccMethod :: !String
  , uccPos :: !Int
  , uccMessage :: !String
  , uccRest :: !ResultSet
  }
  deriving stock (Generic, Show, Eq)

instance NFData UnconsumedColumnE

-- | expected only one row
data ExpectedOneRowE = ExpectedOneRowE
  { oorMethod :: !String
  , oorPos :: !Int
  , oorMessage :: !String
  , oorRest :: !ResultSet
  }
  deriving stock (Generic, Show, Eq)

instance NFData ExpectedOneRowE

-- | expected exactly one row for SelRow
data SqlE = SqlE
  { sInstance :: !String
  , sPos :: !Int
  , sMessage :: !String
  , sRss :: ![ResultSet]
  }
  deriving stock (Generic, Show, Eq)

-- | invalid resultset type: ie returned an update type vs a select type
instance NFData SqlE

-- | invalid result set record type
data UnexpectedResultSetTypeE = UnexpectedResultSetTypeE
  { urstMethod :: !String
  , urstMessage :: !String
  , urstRss :: !ResultSet
  }
  deriving stock (Generic, Show, Eq)

instance NFData UnexpectedResultSetTypeE

-- | no more resultsets were returned but the type definition expected one
data NoResultSetE = NoResultSetE
  { nrMethod :: !String
  , nrPos :: !Int
  , nrMessage :: !String
  }
  deriving stock (Generic, Show, Eq)

instance NFData NoResultSetE

-- | programmer error that shouldn't happen!
data BadE = BadE
  { badMethod :: !String
  , badMessage :: !String
  , badData :: !String
  }
  deriving stock (Generic, Show, Eq)

instance NFData BadE

-- | 'SE'' contains a promoted list of all possible errors
type SE' =
  '[ UpdNE
   , UnconsumedResultSetE
   , SqlE
   , MoreThanOneColumnE
   , UnconsumedColumnE
   , ExpectedOneRowE
   , UnexpectedResultSetTypeE
   , NoResultSetE
   , BadE
   , ConvE
   , DecodingE
   ]

-- | list of all possible errors
type SE = NonEmpty (CoRec V.Identity SE')

-- | extract the short messages from 'SE'
seShortMessages' :: Either SE x -> [String]
seShortMessages' x = x ^.. _Left . traverse . to seShortMessage

-- | extract the short messages from 'SE'
seShortMessages :: SE -> [String]
seShortMessages = map seShortMessage . N.toList

-- | pulls out a shorter message from errors
seShortMessage :: CoRec V.Identity SE' -> String
seShortMessage x =
  let f :: Typeable a => a -> String
      f e = (tyConName . typeRepTyCon . typeOf) e ++ ": "
   in VC.match x $
        VC.H (\e -> f e ++ unMethod e ++ " " ++ unMessage e)
          :& VC.H (\e -> f e ++ ucrsMethod e ++ " " ++ ucrsMessage e)
          :& VC.H (\e -> f e ++ sInstance e ++ " " ++ sMessage e)
          :& VC.H (\e -> f e ++ mtocMethod e ++ " " ++ mtocMessage e)
          :& VC.H (\e -> f e ++ uccMethod e ++ " " ++ uccMessage e)
          :& VC.H (\e -> f e ++ oorMethod e ++ " " ++ oorMessage e)
          :& VC.H (\e -> f e ++ urstMethod e ++ " " ++ urstMessage e)
          :& VC.H (\e -> f e ++ nrMethod e ++ " " ++ nrMessage e)
          :& VC.H (\e -> f e ++ badMethod e ++ " " ++ badMessage e)
          :& VC.H (\e -> f e ++ cvType e ++ " " ++ cvMessage e)
          :& VC.H (\e -> f e ++ deMethod e ++ " " ++ deMessage e)
          :& RNil

-- | lift a decoding error to the larger SE error
liftDE :: DE -> SE
liftDE =
  fmap
    ( weakenCoRec
        . weakenCoRec
        . weakenCoRec
        . weakenCoRec
        . weakenCoRec
        . weakenCoRec
        . weakenCoRec
        . weakenCoRec
        . weakenCoRec
    )

-- | update failed with invalid predicate
failUpdN :: String -> Int -> String -> Either SE a
failUpdN a b c = failSE (UpdNE a b c)

-- | failure because we didnt find exactly one column
failOneCol :: String -> Int -> String -> ResultSet -> Either SE a
failOneCol a b c d = failSE (MoreThanOneColumnE a b c d)

-- | failure because we have more resultsets than expected
failUncRS :: String -> Int -> String -> [ResultSet] -> Either SE a
failUncRS a b c d = failSE (UnconsumedResultSetE a b c d)

-- | failure because we have more columns in a resultset than expected
failUncCol :: String -> Int -> String -> ResultSet -> Either SE a
failUncCol a b c d = failSE (UnconsumedColumnE a b c d)

-- | failure because we have an invalid resultset ie update vs select
failURST :: String -> String -> ResultSet -> Either SE a
failURST a b c = failSE (UnexpectedResultSetTypeE a b c)

-- | failure because we expected exactly one row
failOneRow :: String -> Int -> String -> ResultSet -> Either SE a
failOneRow a b c d = failSE (ExpectedOneRowE a b c d)

-- | failure because didnt find a resultset
failNR :: String -> Int -> String -> Either SE a
failNR a b c = failSE (NoResultSetE a b c)

-- | general failure
failBad :: String -> String -> String -> Either SE a
failBad a b c = failSE (BadE a b c)

-- | lift the failure into 'SE'
failSE :: RElem x SE' (RIndex x SE') => x -> Either SE a
failSE x = Left (pure $ CoRec (V.Identity x))

-- eg hasError @SqlE

-- | 'hasError' checks for errors of a given type p (use typeapplications)
hasError :: forall p a. NatToInt (RIndex p SE') => Either SE a -> Bool
hasError = not . null . getErrors @p

-- | 'getErrors' returns list of errors of given type p (use typeapplications)
getErrors :: forall p a. NatToInt (RIndex p SE') => Either SE a -> [p]
getErrors = either (xes @p) (const [])

-- eg: left (xes @UpdNE) a

-- | pulls out all the messages of a type "t" from a nonempty list of vinyl corecords
xes ::
  forall t ts.
  NatToInt (RIndex t ts) =>
  NonEmpty (CoRec V.Identity ts) ->
  [t]
xes = mapMaybe VC.asA . N.toList

-- | calls 'xes' to see if there were any errors of type "t"
xes' ::
  forall t ts.
  NatToInt (RIndex t ts) =>
  NonEmpty (CoRec V.Identity ts) ->
  Bool
xes' = not . null . xes @t

-- | lifted version 'xes'' over 'Either'
xes'' ::
  forall t ts a.
  NatToInt (RIndex t ts) =>
  Either (NonEmpty (CoRec V.Identity ts)) a ->
  Bool
xes'' (Left v) = xes' @t v
xes'' (Right _) = False
