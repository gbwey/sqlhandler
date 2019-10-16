{-# OPTIONS -Wall -Wcompat -Wincomplete-record-updates -Wincomplete-uni-patterns -Wredundant-constraints #-}
{-# OPTIONS -Wcompat #-}
{-# OPTIONS -Wincomplete-record-updates #-}
{-# OPTIONS -Wincomplete-uni-patterns #-}
{-# OPTIONS -Wredundant-constraints #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE DeriveGeneric #-}
{- |
Module      : Sql
Description : pure functions for describing, prepocessing and postprocessing Sql
Copyright   : (c) Grant Weyburne, 2016
License     : BSD-3
Maintainer  : gbwey9@gmail.com

-}
module Sql
  ( module Sql
  , module One
  , module Database.HDBC
  , module Encoding
  , module Decoding
  , module Conv
  , module Raw
  , module VinylUtils
  ) where
import Raw
import qualified Data.Vinyl.Functor as V
import qualified Data.Vinyl.Core as V
import qualified Data.Vinyl.Recursive as VR
import Data.Vinyl
import qualified Data.Vinyl.CoRec as VC
import Data.Vinyl.CoRec (CoRec(..),weakenCoRec)
import Data.Vinyl.TypeLevel hiding (Nat)
import Control.Arrow
import Control.Lens hiding (rmap,Identity,Const,op)
import Data.Proxy
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)
import GHC.TypeLits (ErrorMessage((:<>:),(:$$:)),Nat,KnownNat)
import qualified GHC.TypeLits as GL
import Control.Monad
import One
import Database.HDBC (SqlValue(..), SqlColDesc(..))
import Database.HDBC.ColTypes (SqlTypeId (SqlUnknownT))
import Conv
import PredState
import PredHelper
import Decoding
import Encoding
import Text.Shakespeare.Text
import Data.String
import Data.Text.Internal.Builder
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as N
import Data.List (intercalate)
import GHC.Generics (Generic)
import Data.Typeable
import VinylUtils
import Data.Kind (Type)
import qualified PCombinators as P
import PCombinators ((:.:), type (~>), Apply)

-- | 'Sql' is the core ADT that holds a vinyl record of encoders for the input
-- and a vinyl record of decoders for the ouput and then the Sql text
data Sql db a b = Sql { _sDescription :: !Text -- ^ description used for logging and error handling
                      , _sEncoders :: !(Rec Enc a) -- ^ a record of encoders matching the input parameters
                      , _sDecoders :: !(Rec SingleIn b) -- ^ a record of decoders matching the output columns
                      , _sSql :: !Text } -- ^ sql

-- makeLenses ''Sql -- write out the lens instances manually

sDescription :: Lens' (Sql db a b) Text
sDescription afa s = (\d' -> s { _sDescription = d' }) <$> afa (_sDescription s)

sEncoders :: Lens (Sql db a b) (Sql db a' b) (Rec Enc a) (Rec Enc a')
sEncoders afa s = (\d' -> s { _sEncoders = d' }) <$> afa (_sEncoders s)

sDecoders :: Lens (Sql db a b) (Sql db a b') (Rec SingleIn b) (Rec SingleIn b')
sDecoders afa s = (\d' -> s { _sDecoders = d' }) <$> afa (_sDecoders s)

sSql :: Lens' (Sql db a b) Text
sSql afa s = (\d' -> s { _sSql = d' }) <$> afa (_sSql s)

instance ToText (Sql db a b) where
  toText sql = fromText (_sSql sql)

instance Show (Sql db a b) where
  show = T.unpack . _sSql

pattern I :: a -> V.Identity a
pattern I a = V.Identity a

type PredZ a = Pred [String] a

-- | 'SE'' contains a promoted list of all possible errors
type SE' = '[ UpdNE
            , UnconsumedColE
            , SingleColE
            , UnexpectedResultSetTypeE
            , NoResultSetE
            , BadE
            , PredE
            , PredExceptionE
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
       (VC.H $ \e -> f e ++ _unMethod e ++ " " ++ _unMessage e)
    :& (VC.H $ \e -> f e ++ _uccMethod e ++ " " ++ _uccMessage e)
    :& (VC.H $ \e -> f e ++ _sicInstance e ++ " " ++ _sicMessage e)
    :& (VC.H $ \e -> f e ++ _urstMethod e ++ " " ++ _urstMessage e)
    :& (VC.H $ \e -> f e ++ _nrMethod e ++ " " ++ _nrMessage e)
    :& (VC.H $ \e -> f e ++ _badMethod e ++ " " ++ _badMessage e)
    :& (VC.H $ \e -> f e ++ _peErr e)
    :& (VC.H $ \e -> f e ++ intercalate " : " (N.toList (_peExceptionE e)) ++ " " ++ _peErrE e)
    :& (VC.H $ \e -> f e ++ _cvType e ++ " " ++ _cvMessage e)
    :& (VC.H $ \e -> f e ++ _deMethod e ++ " " ++ _deMessage e)
    :& RNil

-- | lift a predicate error to the larger SE error
liftPE :: PE -> SE
liftPE = \case
  Left p -> CoRec (V.Identity p) :| []
  Right p -> CoRec (V.Identity p) :| []

-- | lift a decoding error to the larger SE error
liftDE :: DE -> SE
liftDE = fmap (weakenCoRec
             . weakenCoRec
             . weakenCoRec
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

--failSE :: RecElem Rec x x SE' SE' (RIndex x SE') => x -> Either SE a
failSE :: RElem x SE' (RIndex x SE') => x -> Either SE a
failSE x = Left (CoRec (V.Identity x) :| [])

type RMeta = [(String, SqlColDesc)]
type ResultSet = Either Int (RMeta,[[SqlValue]])

hMetaNull :: (String, SqlColDesc)
hMetaNull = ("", SqlColDesc (SqlUnknownT "dummy type:hMetaNull") (Just 5) (Just 7) (Just 11) (Just True))

data UpdNE = UpdNE { _unMethod :: !String, _unPos :: !Int, _unMessage :: !String } deriving (Generic, Show, Eq)
data UnconsumedColE = UnconsumedColE { _uccMethod :: !String, _uccPos :: !Int, _uccMessage :: !String, _uccRest :: ![ResultSet] } deriving (Generic, Show, Eq)
data SingleColE = SingleColE { _sicInstance :: !String, _sicPos :: !(Maybe Int), _sicMessage :: !String, _sicRss :: ![ResultSet] } deriving (Generic, Show, Eq)
data UnexpectedResultSetTypeE = UnexpectedResultSetTypeE { _urstMethod :: !String, _urstMessage :: !String, _urstRss :: !ResultSet } deriving (Generic, Show, Eq)
data NoResultSetE = NoResultSetE { _nrMethod :: !String, _nrPos :: !Int, _nrMessage :: !String } deriving (Generic, Show, Eq)
data BadE = BadE { _badMethod :: !String, _badMessage :: !String, _badData :: !String } deriving (Generic, Show, Eq)

-- eg: left (xes @UpdNE) a
-- | 'xes' pulls out all the messages of a type t from a nonempty list of vinyl corecords
xes :: forall t ts . NatToInt (RIndex t ts) => NonEmpty (CoRec V.Identity ts) -> [t]
xes = mapMaybe VC.asA . N.toList

xes' :: forall t ts . NatToInt (RIndex t ts) => NonEmpty (CoRec V.Identity ts) -> Bool
xes' = not . null . xes @t

xes'' :: forall t ts a . NatToInt (RIndex t ts) => Either (NonEmpty (CoRec V.Identity ts)) a -> Bool
xes'' (Left v) = xes' @t v
xes'' (Right _) = False

showSE :: SE -> String
showSE = intercalate "\n" . map show . N.toList

-- | 'ST' is a simple state applicative
newtype ST e s a = ST { unST :: s -> Either e (s, a) }

instance Functor (ST e s) where
  fmap f (ST g) = ST $ \s -> case g s of
                               Left e -> Left e
                               Right (s', a) -> Right (s', f a)
instance Applicative (ST e s) where
  pure a = ST $ \s -> Right (s, a)
  ST sab <*> ST sa = ST $ \s -> case sab s of
                                  Left e -> Left e
                                  Right (s', ab) -> case sa s' of
                                                     Left e -> Left e
                                                     Right (s'', a) -> Right (s'', ab a)

data Op = OPLT | OPLE | OPEQ | OPGE | OPGT | OPNE deriving (Show, Eq)

-- | 'UpdN' is similar to 'Upd' but encodes a type level predicate on the return code
newtype UpdN (op :: Op) (val :: Nat) = UpdN { unUpdN :: Int } deriving (Show, Eq, Num, Ord, Generic)

-- | 'Upd' holds a return code from a sql update call
newtype Upd = Upd { unUpd :: Int } deriving (Show, Eq, Num, Ord, Generic)

-- | 'SelOne' holds a single row of data
newtype SelOne a = SelOne { unSelOne :: a } deriving (Show, Eq, Generic)

-- | 'Sel' holds a zero or more rows of data
newtype Sel a = Sel { unSel :: [a] } deriving (Show, Eq, Generic)

-- | 'Alle' is a higher order function that holds zero or more resultsets of the same type
newtype Alle a = Alle { unAlle :: [a] } deriving (Show, Eq, Generic)

-- | ':+:' is a higher order function that holds a resultset of type a or of type b
newtype a :+: b = EitherRS { unEitherRS :: Either a b } deriving (Show, Eq, Generic)

-- | 'Some' is a higher order function that is similar to 'Alle' but holds exactly n resultsets of the same type
--   if rev = 'True then will grab the last n resultsets instead of the first n resultsets
newtype Some (rev :: Bool) (n :: Nat) a = Some { unSome :: [a] } deriving (Show, Eq, Generic)

type SomeT (n :: Nat) = Some 'False n
type EmosT (n :: Nat) = Some 'True n

type AnyRaw = Upd :+: SelRaw

-- | 'SelRaw' holds a query resultset with raw data
newtype SelRaw = SelRaw { unSelRaw :: [[SqlValue]] } deriving (Show, Eq, Generic)

-- | 'Single' represents a single result set where 'SingleIn' is the input and 'SingleOut' is the
class Single a where
  data SingleIn a
  type SingleOut a
  showF :: SingleIn a -> String
  singleCol :: SingleIn a -> (Int, [ResultSet]) -> Either SE ((Int, [ResultSet]), ([RMeta],(a, SingleOut a)))

-- | 'Upd' represents any non select query eg dml insert / update / delete o ddl create / drop / alter
instance Single Upd where
  data SingleIn Upd = UpdP (PredZ Int)
  type SingleOut Upd = Int
  showF (UpdP p) = "UpdP " ++ show p

  singleCol (UpdP p) (pos,rss) =
    case rss of
      rs1:rss' -> left (CoRec (V.Identity (SingleColE "Upd" (Just pos) "" rss)) N.<|) $ do
        rc <- updImpl rs1 p
        return ((pos+1,rss'), ([], (Upd rc,rc)))
      [] -> failNR "Upd" pos "no resultset"

instance (ShowOp op, KnownNat val) => Single (UpdN (op :: Op) (val :: Nat)) where
  data SingleIn (UpdN op val) = UpdNP
  type SingleOut (UpdN op val) = Int
  showF _ = "UpdNP " ++ show (getPredOp @op (P.pnat @val))

  singleCol UpdNP (pos,rss) =
    case rss of
      rs1:rss' -> left (CoRec (V.Identity (SingleColE "UpdN" (Just pos) "" rss)) N.<|) $ do
        let p = getPredOp @op (P.pnat @val)
        rc <- updImpl rs1 p
        return ((pos+1,rss'), ([], (UpdN rc,rc)))
      [] -> failNR "UpdN" pos "no resultset"

instance Show a => Single (SelOne a) where
  data SingleIn (SelOne a) = SelOneP (PredZ a) (Dec a)
  type SingleOut (SelOne a) = a
  showF (SelOneP p d) = "SelOneP " ++ show p ++ " " ++ show d

  singleCol (SelOneP p dec) (pos,rss) =
    case rss of
       rs1:rss' -> left (CoRec (V.Identity (SingleColE "SelOne" (Just pos) "" rss)) N.<|) $ do
         (meta,xxs) <- selImpl rs1 (PForAll p) dec
         case xxs of
           [xs] -> return ((pos+1,rss'), ([meta], (SelOne xs,xs)))
           _ -> failSIC "SelOne" (Just pos) ("expected 1 row but found " ++ show (length xxs)) rss
       [] -> failNR "SelOne" pos "no resultset"

instance Show a => Single (Sel a) where
  data SingleIn (Sel a) = SelP (PredZ [a]) (Dec a)
  type SingleOut (Sel a) = [a]
  showF (SelP p d) = "SelP " ++ show p ++ " " ++ show d

  singleCol (SelP p dec) (pos,rss) =
    case rss of
       rs1:rss' -> left (CoRec (V.Identity (SingleColE "Sel" (Just pos) "" rss)) N.<|) $ do
          (meta,a) <- selImpl rs1 p dec
          return ((pos+1,rss'), ([meta],(Sel a,a)))
       [] -> failNR "Sel" pos "no resultset"

instance (Single a, Show a) => Single (Alle a) where
  data SingleIn (Alle a) = AlleP (SingleIn a) (PredZ [a])
  type SingleOut (Alle a) = [SingleOut a]
  showF (AlleP a p) = "AlleP " ++ show a ++ " " ++ show p

  singleCol (AlleP one p) (pos,rss) =
    let n = length rss
    in left (CoRec (V.Identity (SingleColE "Alle" (Just pos) (show n ++ " resultsets") rss)) N.<|) $ do
         ((ret,wret),rssout) <- foldM (\((as,was),rss') i -> do
                                                 ((i',rss''),(hm,(a,wa))) <- singleCol one (i,rss')
                                                 unless (i' == i+1) $ failBad "Alle Col" ("pos=" ++ show pos ++ " should not happen: not getting a single resultset! " ++ show (i,i')) ""
                                                 return ((as++[(hm,a)],was++[wa]), rss'')
                              ) (([],[]), rss) [1.. n]
         left (\e -> CoRec (V.Identity (SingleColE "Alle" (Just pos) "predicate failure" rss)) N.<| liftPE e) $ runPred p (map snd ret)
         return ((pos+n,rssout), ([], (Alle (map snd ret), wret)))

instance (Single a, Single b) => Single (a :+: b) where
  data SingleIn (a :+: b) = SingleIn a :+: SingleIn b
  type SingleOut (a :+: b) = Either (SingleOut a) (SingleOut b)
  showF (a :+: b) = show a ++ " :+: " ++ show b

  -- try the first: if it fails try the second using the same resultset
  singleCol (a :+: b) (pos,rss) =
    left (CoRec (V.Identity (SingleColE "EitherRS" (Just pos) "" rss)) N.<|) $
      case singleCol a (pos,rss) of
        Left e1 -> case singleCol b (pos,rss) of
                    Left e2 -> Left $ CoRec (V.Identity (SingleColE "EitherRS" (Just pos) "failed trying Left and Right" rss)) N.<| (e1 <> e2)
                    Right (ret, (hm,(a2,wa2))) -> Right (ret, (hm, (EitherRS (Right a2),Right wa2)))
        Right (ret, (hm,(a1,wa1))) -> Right (ret, (hm, (EitherRS (Left a1),Left wa1)))

instance (Single a, Show a, P.GetBool rev, KnownNat n) => Single (Some (rev :: Bool) (n :: Nat) a) where
  data SingleIn (Some rev n a) = SomeP (SingleIn a) (PredZ [a])
  type SingleOut (Some rev n a) = [SingleOut a]
  showF (SomeP a p) = "SomeP" ++ (if P.getBool @rev then " Reverse" else "") ++ "(" ++ show (P.pnat @n) ++  ") " ++ show a ++ " " ++ show p

  singleCol w@(SomeP one p) (pos,rss) =
     case someImpl w rss of
       Left e -> Left e
       Right (n,msg) -> do
         left (CoRec (V.Identity (SingleColE msg (Just pos) "" rss)) N.<|) $ do
           ((ret,wret),rssout) <- foldM (\((as,was),rss') i -> do
                                                   ((i',rss''),(hm,(a,wa))) <- singleCol one (i,rss')
                                                   unless (i' == i+1) $ failBad (msg ++ " Col") ("pos=" ++ show pos ++ " should not happen: not getting a single resultset!" ++ show (i,i')) ""
                                                   return ((as++[(hm,a)],was++[wa]), rss'')
                                ) (([],[]), rss) [1.. n]
           left (\e -> CoRec (V.Identity (SingleColE msg (Just pos) "predicate failure" rss)) N.<| liftPE e) $ runPred p (map snd ret)
           return ((pos+n,rssout), ([], (Some (map snd ret),wret)))

someImpl :: forall rev n a b . (P.GetBool rev, KnownNat n) => SingleIn (Some rev n a) -> [b] -> Either SE (Int, String)
someImpl _ rss =
    let rev = P.getBool @rev
        n' = P.pnat @n
        n = if rev then length rss - n' else n'
        msg = "Some " ++ (if rev then "Reverse " else "") ++ show n'
    in if | n<0 -> failNR msg n ("not enough resultsets(" ++ show (length rss) ++ ") n=" ++ show n')
          | n>length rss -> failNR msg n ("not enough resultsets(" ++ show (length rss) ++ ") n=" ++ show n')
          | otherwise -> return (n,msg)

instance Single SelRaw where
  data SingleIn SelRaw = SelRawP (PredZ [[SqlValue]])
  type SingleOut SelRaw = [[SqlValue]]
  showF (SelRawP p) = "SelRawP " ++ show p

  singleCol (SelRawP p) (pos,rss) =
    case rss of
      rs1:rss' -> left (CoRec (V.Identity (SingleColE "SelRaw" (Just pos) "" rss)) N.<|) $ do
        (meta,a) <- selRawImpl rs1 p
        return ((pos+1,rss'), ([meta], (SelRaw a,a)))
      [] -> failNR "Sel" pos "no resultset"

instance Single a => Show (SingleIn a) where
  show = showF

-- | 'ZZZ' holds the input/ouput for a given resultset
data ZZZ a = ZZZ { _zzz1 :: !(SingleIn a), _zzz2 :: a, _zzz3 :: SingleOut a, _zzz4 :: ![RMeta] }

-- | Lens for accessing the predicate for 'ZZZ'
zzz1 :: Lens' (ZZZ a) (SingleIn a)
zzz1 afb z = (\x -> z { _zzz1 = x}) <$> afb (_zzz1 z)

-- | Lens for accessing the wrapped output value for 'ZZZ'
zzz2 :: Lens' (ZZZ a) a
zzz2 afb z = (\x -> z { _zzz2 = x}) <$> afb (_zzz2 z)

-- | Lens for accessing the unwrapped output value for 'ZZZ'
zzz3 :: Lens' (ZZZ a) (SingleOut a)
zzz3 afb z = (\x -> z { _zzz3 = x}) <$> afb (_zzz3 z)

-- | Lens for accessing the meta data for 'ZZZ'
zzz4 :: Lens' (ZZZ a) [RMeta]
zzz4 afb z = (\x -> z { _zzz4 = x}) <$> afb (_zzz4 z)

deriving instance (Show a, Show (SingleIn a), Show (SingleOut a)) => Show (ZZZ a)

-- bearbeiten: how to get around using undefined
-- | 'toZZZ' sets up the initial state before processing all the resultsets
toZZZ :: Rec SingleIn rs -> Rec ZZZ rs
toZZZ = VR.rmap $ \xa -> ZZZ xa undefined undefined []

-- | 'ShowOp' extracts a value level predicate from the typelevel for 'UpdN'
class ShowOp (a :: Op) where
  getPredOp :: Int -> PredZ Int
instance ShowOp 'OPLT where
  getPredOp = plt
instance ShowOp 'OPLE where
  getPredOp = ple
instance ShowOp 'OPEQ where
  getPredOp = peq
instance ShowOp 'OPGE where
  getPredOp = pge
instance ShowOp 'OPGT where
  getPredOp = pgt
instance ShowOp 'OPNE where
  getPredOp = pne

processRetCol
  :: (RecAll ZZZ rs SingleZ
    , ValidateNested rs
    ) => Rec SingleIn rs -> [ResultSet] -> Either SE (Rec ZZZ rs)
processRetCol decRec = processRetCol' (toZZZ decRec)

-- | 'processRetCol'' is the same as 'processRet'' but additionally includes metadata
processRetCol'
  :: (RecAll ZZZ rs SingleZ
    , ValidateNested rs
    ) =>
     Rec ZZZ rs -> [ResultSet] -> Either SE (Rec ZZZ rs)
processRetCol' xs rss =
  case flip unST (0::Int,rss) $ rtraverse (\(V.Compose (V.Dict x)) -> ST (singleColZ x)) (VR.reifyConstraint (Proxy @SingleZ) xs) of
    Left e -> Left e
    Right ((pos,rss'), w) | null rss' -> Right w
                          | otherwise -> failUCC "processRetCol'" pos "Unconsumed" rss'

-- | 'SingleZ' is a simple state monad wrapper around 'Single' which processes a single 'ZZZ' entry
-- we need this wrapper as vinyl uses Rec ZZZ rs wraps ZZZ around each rs
class SingleZ a where
  singleColZ :: a -> (Int, [ResultSet]) -> Either SE ((Int, [ResultSet]), a)

-- only one instance that wraps 'Single' which is needed for 'processRet' and 'processRetCol'
instance Single a => SingleZ (ZZZ a) where
  singleColZ w (b,c) = case singleCol (_zzz1 w) (b,c) of
                                     Left e -> Left e
                                     Right (z, (hms,(a,wa))) -> Right (z,w { _zzz2 = a, _zzz3 = wa, _zzz4 = hms })

-- | shortcut to get all the hmetas in one go
hmall :: Rec ZZZ '[a] -> [RMeta]
-- cant use Vinyl Const cos no applicative instance!!!!
--hmall rs = L.getConst $ rtraverse (L.Const . _zzz4) rs
hmall = VR.rfoldMap _zzz4

-- need this to do the conversions
-- max of 8
--type family SingleOuts (rs :: [Type]) = w | w -> rs where  -- doesnt work cos overlaps
-- | convert up to 8 resultsets to tuples
type family SingleOuts (rs :: [Type]) :: Type where
  SingleOuts '[] = ()
  SingleOuts '[a] = SingleOut a
  SingleOuts '[a,b] = (SingleOut a, SingleOut b)
  SingleOuts '[a,b,c] = (SingleOut a, SingleOut b, SingleOut c)
  SingleOuts '[a,b,c,d] = (SingleOut a, SingleOut b, SingleOut c, SingleOut d)
  SingleOuts '[a,b,c,d,e] = (SingleOut a, SingleOut b, SingleOut c, SingleOut d, SingleOut e)
  SingleOuts '[a,b,c,d,e,f] = (SingleOut a, SingleOut b, SingleOut c, SingleOut d, SingleOut e, SingleOut f)
  SingleOuts '[a,b,c,d,e,f,g] = (SingleOut a, SingleOut b, SingleOut c, SingleOut d, SingleOut e, SingleOut f, SingleOut g)
  SingleOuts '[a,b,c,d,e,f,g,h] = (SingleOut a, SingleOut b, SingleOut c, SingleOut d, SingleOut e, SingleOut f, SingleOut g, SingleOut h)


-- makes use of unique structure of vinyl record or at least the rs part of Rec f rs
-- no problem cos going the right direction from rs to v which is well defined
-- going the other way is problematic
-- max of 8

-- | convert Rec ZZZ rs to tuple
class PGen (rs :: [Type]) (v :: Type) | rs -> v where
  ext :: Rec ZZZ rs -> SingleOuts rs
instance PGen '[] () where
  ext RNil = ()
instance PGen '[a] a where
  ext (r1 :& RNil) = _zzz3 r1
instance PGen '[a,b] (a,b) where
  ext (r1 :& r2 :& RNil) = (_zzz3 r1, _zzz3 r2)
instance PGen '[a,b,c] (a,b,c) where
  ext (r1 :& r2 :& r3 :& RNil) = (_zzz3 r1, _zzz3 r2, _zzz3 r3)
instance PGen '[a,b,c,d] (a,b,c,d) where
  ext (r1 :& r2 :& r3 :& r4 :& RNil) = (_zzz3 r1, _zzz3 r2, _zzz3 r3, _zzz3 r4)
instance PGen '[a,b,c,d,e] (a,b,c,d,e) where
  ext (r1 :& r2 :& r3 :& r4 :& r5 :& RNil) = (_zzz3 r1, _zzz3 r2, _zzz3 r3, _zzz3 r4, _zzz3 r5)
instance PGen '[a,b,c,d,e,f] (a,b,c,d,e,f) where
  ext (r1 :& r2 :& r3 :& r4 :& r5 :& r6 :& RNil) = (_zzz3 r1, _zzz3 r2, _zzz3 r3, _zzz3 r4, _zzz3 r5, _zzz3 r6)
instance PGen '[a,b,c,d,e,f,g] (a,b,c,d,e,f,g) where
  ext (r1 :& r2 :& r3 :& r4 :& r5 :& r6 :& r7 :& RNil) = (_zzz3 r1, _zzz3 r2, _zzz3 r3, _zzz3 r4, _zzz3 r5, _zzz3 r6, _zzz3 r7)
instance PGen '[a,b,c,d,e,f,g,h] (a,b,c,d,e,f,g,h) where
  ext (r1 :& r2 :& r3 :& r4 :& r5 :& r6 :& r7 :& r8 :& RNil) = (_zzz3 r1, _zzz3 r2, _zzz3 r3, _zzz3 r4, _zzz3 r5, _zzz3 r6, _zzz3 r7, _zzz3 r8)

-- nested tuples: not as useful as PGen/ext but works for all sizes!
-- () (a,()) (a,(b,())) (a,(b,(c,()))) (a,(b,(c,(d,())))) etc! a lot yurky!
type family SingleOuts' (rs :: [Type]) :: Type where
  SingleOuts' '[] = ()
  SingleOuts' (a ': as) = (SingleOut a, SingleOuts' as)

-- | alternative approach of converting vinyl record of results to a nested tuple
class PGen' (rs :: [Type]) where
  ext' :: Rec ZZZ rs -> SingleOuts' rs
instance PGen' '[] where
  ext' RNil = ()
instance PGen' as => PGen' (a ': as) where
  ext' (r :& rs) = (_zzz3 r, ext' rs)

-- | 'selImpl' tries to decode a result set based on the decoder and then runs the predicate
selImpl :: Show a => ResultSet -> PredZ [a] -> Dec a -> Either SE (RMeta, [a])
selImpl z@(Right (meta,xxs)) p (Dec dec) = do
  ys <- forM (zip [1::Int ..] xxs) $ \(r,xs) -> do
          case dec xs of
            Left es ->
              case snd $ getDecErrors es of
                [] -> error "selImpl: missing DecodingE"
                ss:_ -> let c = length xs - length (_deSqlValues ss)
                        in Left $ liftDE $ decAddError' "selImpl" ("row/col " ++ show (r,c)) [] es

            Right a -> return a
  ret <- forM (zip [1::Int ..] ys) $ \(i,(a,lft)) -> do
    unless (null lft) $ failUCC "Select" i ("selImpl: didnt consume all values! row " ++ show i ++ ":leftovers=" ++ show lft) [z]
    return a
  left (\e -> CoRec (V.Identity (SingleColE "Select" Nothing "selImpl: predicate failure" [z])) N.<| liftPE e) $ runPred p ret
  return (meta, ret)
selImpl z _ _ = failURST "selImpl(Select)" "expected a select but found an update" z

-- | 'updImpl' processes an Update resultset and runs the predicate
updImpl :: ResultSet -> PredZ Int -> Either SE Int
updImpl z@(Left i) p = left (\e -> CoRec (V.Identity (SingleColE "Update" Nothing "updImpl: predicate failure" [z])) N.<| liftPE e) (i <$ runPred p i)
updImpl z _ = failURST "updImpl(Update)" "expected an update but found a select" z

-- | 'selRawImpl' processes a Query resultset and runs the predicate
selRawImpl :: ResultSet -> PredZ [[SqlValue]] -> Either SE (RMeta, [[SqlValue]])
selRawImpl z@(Right (meta,xs)) p = left (\e -> CoRec (V.Identity (SingleColE "SelRaw" Nothing "selRawImpl: predicate failure" [z])) N.<| liftPE e) ((meta,xs) <$ runPred p xs)
selRawImpl z _ = failURST "selRawImpl(Select)" "expected a select but found an update" z

-- | 'IsString' instance for 'Sql'
instance (DefEnc (Rec Enc a), DefDec (Rec SingleIn b)) => IsString (Sql db a b) where
  fromString s = mkSql "<fromString>" (T.pack s)

unsafeCoerceSql :: Text -> Rec Enc a -> Rec SingleIn b -> Sql db1 ax bx -> Sql db2 a b
unsafeCoerceSql str e d (Sql desc _ _ sql) = Sql (str <> "[" <> desc <> "]:coerced") e d sql

type ISql db a b = Int -> (Sql db a b, Int)

-- | 'ToTuple' converts Rec V.Identity to tuples
--   single Tuple is a special case.
--   pulls Rec L.Identity (r1,r2,...) into (r1,r2,...)
class ToTuple a b | a -> b, b -> a where
  fromRec :: b -> a
  toRec :: a -> b
instance ToTuple () (Rec V.Identity '[]) where
  fromRec RNil = ()
  toRec () = RNil
instance ToTuple (One a1) (Rec V.Identity '[a1]) where
  fromRec (V.Identity a1 :& RNil) = One a1
  toRec (One a1) = V.Identity a1 :& RNil
instance ToTuple (a1,a2) (Rec V.Identity '[a1,a2]) where
  fromRec (V.Identity a1 :& V.Identity a2 :& RNil) = (a1,a2)
  toRec (a1,a2) = V.Identity a1 :& V.Identity a2 :& RNil
instance ToTuple (a1,a2,a3) (Rec V.Identity '[a1,a2,a3]) where
  fromRec (V.Identity a1 :& V.Identity a2 :& V.Identity a3 :& RNil) = (a1,a2,a3)
  toRec (a1,a2,a3) = V.Identity a1 :& V.Identity a2 :& V.Identity a3 :& RNil
instance ToTuple (a1,a2,a3,a4) (Rec V.Identity '[a1,a2,a3,a4]) where
  fromRec (V.Identity a1 :& V.Identity a2 :& V.Identity a3 :& V.Identity a4 :& RNil) = (a1,a2,a3,a4)
  toRec (a1,a2,a3,a4) = V.Identity a1 :& V.Identity a2 :& V.Identity a3 :& V.Identity a4 :& RNil
instance ToTuple (a1,a2,a3,a4,a5) (Rec V.Identity '[a1,a2,a3,a4,a5]) where
  fromRec (V.Identity a1 :& V.Identity a2 :& V.Identity a3 :& V.Identity a4 :& V.Identity a5 :& RNil) = (a1,a2,a3,a4,a5)
  toRec (a1,a2,a3,a4,a5) = V.Identity a1 :& V.Identity a2 :& V.Identity a3 :& V.Identity a4 :& V.Identity a5 :& RNil
instance ToTuple (a1,a2,a3,a4,a5,a6) (Rec V.Identity '[a1,a2,a3,a4,a5,a6]) where
  fromRec (V.Identity a1 :& V.Identity a2 :& V.Identity a3 :& V.Identity a4 :& V.Identity a5 :& V.Identity a6 :& RNil) = (a1,a2,a3,a4,a5,a6)
  toRec (a1,a2,a3,a4,a5,a6) = V.Identity a1 :& V.Identity a2 :& V.Identity a3 :& V.Identity a4 :& V.Identity a5 :& V.Identity a6 :& RNil
instance ToTuple (a1,a2,a3,a4,a5,a6,a7) (Rec V.Identity '[a1,a2,a3,a4,a5,a6,a7]) where
  fromRec (V.Identity a1 :& V.Identity a2 :& V.Identity a3 :& V.Identity a4 :& V.Identity a5 :& V.Identity a6 :& V.Identity a7 :& RNil) = (a1,a2,a3,a4,a5,a6,a7)
  toRec (a1,a2,a3,a4,a5,a6,a7) = V.Identity a1 :& V.Identity a2 :& V.Identity a3 :& V.Identity a4 :& V.Identity a5 :& V.Identity a6 :& V.Identity a7 :& RNil
instance ToTuple (a1,a2,a3,a4,a5,a6,a7,a8) (Rec V.Identity '[a1,a2,a3,a4,a5,a6,a7,a8]) where
  fromRec (V.Identity a1 :& V.Identity a2 :& V.Identity a3 :& V.Identity a4 :& V.Identity a5 :& V.Identity a6 :& V.Identity a7 :& V.Identity a8 :& RNil) = (a1,a2,a3,a4,a5,a6,a7,a8)
  toRec (a1,a2,a3,a4,a5,a6,a7,a8) = V.Identity a1 :& V.Identity a2 :& V.Identity a3 :& V.Identity a4 :& V.Identity a5 :& V.Identity a6 :& V.Identity a7 :& V.Identity a8 :& RNil
instance ToTuple (a1,a2,a3,a4,a5,a6,a7,a8,a9) (Rec V.Identity '[a1,a2,a3,a4,a5,a6,a7,a8,a9]) where
  fromRec (V.Identity a1 :& V.Identity a2 :& V.Identity a3 :& V.Identity a4 :& V.Identity a5 :& V.Identity a6 :& V.Identity a7 :& V.Identity a8 :& V.Identity a9 :& RNil) = (a1,a2,a3,a4,a5,a6,a7,a8,a9)
  toRec (a1,a2,a3,a4,a5,a6,a7,a8,a9) = V.Identity a1 :& V.Identity a2 :& V.Identity a3 :& V.Identity a4 :& V.Identity a5 :& V.Identity a6 :& V.Identity a7 :& V.Identity a8 :& V.Identity a9 :& RNil
instance ToTuple (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) (Rec V.Identity '[a1,a2,a3,a4,a5,a6,a7,a8,a9,a10]) where
  fromRec (V.Identity a1 :& V.Identity a2 :& V.Identity a3 :& V.Identity a4 :& V.Identity a5 :& V.Identity a6 :& V.Identity a7 :& V.Identity a8 :& V.Identity a9 :& V.Identity a10 :& RNil) = (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10)
  toRec (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) = V.Identity a1 :& V.Identity a2 :& V.Identity a3 :& V.Identity a4 :& V.Identity a5 :& V.Identity a6 :& V.Identity a7 :& V.Identity a8 :& V.Identity a9 :& V.Identity a10 :& RNil

-- adds a delimiter ; cos required by postgres (only mssql and postgres support multiple resultsets)
-- | combines two queries as separate resultsets
sqlCombine :: Sql db a1 b1 -> Sql db a2 b2 -> Sql db (a1 ++ a2) (b1 ++ b2)
sqlCombine (Sql desc1 a1 b1 q1) (Sql desc2 a2 b2 q2) =
  Sql ("[" <> desc1 <> "][" <> desc2 <> "]")  (rappend a1 a2) (rappend b1 b2) (q1 <> ";\n" <> q2)

-- [*] fails so use [Type]  -- Type == * (* and Type are in Data.Kind)
-- flipped around so DefDec is first: runSql sets the defEnc so defDec is more important
-- runSql usually determines DefEnc so we only need specify DefDec
-- explicit signatures so we can move 'k' away from from first 2 positions
-- not true: k doesnt matter
-- | helper methods for defining Sql but using defaults for encoding and decoding
mkSql' :: -- forall (b :: [Type]) (a :: [Type]) k (db :: k).
     (DefDec (Rec SingleIn b), DefEnc (Rec Enc a)) =>
     Text -> Sql db a b
mkSql' = mkSql "sqlDef"

mkSql :: -- forall (b :: [Type]) (a :: [Type]) k (db :: k).
     (DefDec (Rec SingleIn b), DefEnc (Rec Enc a)) =>
     Text -> Text -> Sql db a b
mkSql s = Sql s defEnc defDec

-- bearbeiten: still need a sensible way to compose all this stuff
-- should be able to choose the dec and pred and then do the next result set

-- remember have to have a Dec forall of 'a' so have to provide a list of up to 8 Decs
-- use defD @String not defDec ...
-- not sure how useful this
selWithDec :: forall rs
   . DecList rs
   => Rec Dec rs
   -> Rec SingleIn '[Sel (DecTuples rs)]
selWithDec d = E1 (SelP 1 (decList d))

selOneWithDec :: forall rs
   . DecList rs
   => Rec Dec rs
   -> Rec SingleIn '[SelOne (DecTuples rs)]
selOneWithDec d = E1 (SelOneP 1 (decList d))

-- provide a vinyl list of Decs and then it will convert to the sized tuple
-- of course each defDec must have a type! eg defDec @(Dec String)
-- or better defD @String
-- fd $ runSql pglocal RNil $ selSqlWithDec  (decIp :& defD @String :& RNil) "select '124.11.123.45 xx','abc'"
selSqlWithDec :: forall rs a db
   . (DecList rs, DefEnc (Rec Enc a))
   => Rec Dec rs
   -> Text
   -> Sql db a '[Sel (DecTuples rs)]
selSqlWithDec d = Sql "selSqlWithDec" defEnc (E1 (SelP 1 (decList d)))

selOneSqlWithDec :: forall rs a db
   . (DecList rs, DefEnc (Rec Enc a))
   => Rec Dec rs
   -> Text
   -> Sql db a '[SelOne (DecTuples rs)]
selOneSqlWithDec d = Sql "selOneSqlWithDec" defEnc (E1 (SelOneP 1 (decList d)))

-- you provide a vinyl list of decs and then it will work for a vinyl list result
selSqlWithDecW :: forall rs a db
   . (DecW rs, DefEnc (Rec Enc a))
   => Rec Dec rs
   -> Text
   -> Sql db a '[Sel (WW rs)]
selSqlWithDecW d = Sql "selSqlWithDecW" defEnc (E1 (SelP 1 (decW d)))

selOneSqlWithDecW :: forall rs a db
   . (DecW rs, DefEnc (Rec Enc a))
   => Rec Dec rs
   -> Text
   -> Sql db a '[SelOne (WW rs)]
selOneSqlWithDecW d = Sql "selOneSqlWithDecW" defEnc (E1 (SelOneP 1 (decW d)))

-- for ElFields but you provide a vinyl dec list: still have to have rs defined
-- fd $ runSql pglocal RNil $ selSqlWithDecH @'["aa" ::: IP Int, "bb" ::: String] (decIp :& defD :& RNil) "select '124.11.123.45 xx','abc'"
selSqlWithDecH :: forall rs a db
   . (DecH rs, DefEnc (Rec Enc a))
   => Rec Dec (P.Map P.SndSym0 rs)
   -> Text
   -> Sql db a '[Sel (Rec ElField rs)]
selSqlWithDecH d = Sql "selSqlWithDecH" defEnc (E1 (SelP 1 (decH d)))

-- you provide just the rs with ==: ie labels and all and it figures out the rs result
-- fd $ runSql pglocal RNil $ selSqlWithDecI (#xx ==: decIp :& #yy ==: defD @String :& RNil) "select '124.11.123.45 xx','abc'"
selSqlWithDecI :: forall rs a db
   . (DecI rs, DefEnc (Rec Enc a))
   => Rec ElFieldDec rs
   -> Text
   -> Sql db a '[Sel (Rec ElField rs)]
selSqlWithDecI d = Sql "selSqlWithDecI" defEnc (E1 (SelP 1 (decI d)))

selWithPred :: forall a
   . DefDec (Dec a)
   => PredZ [a]
   -> Rec SingleIn '[Sel a]
selWithPred p = E1 (SelP p defDec)

selOneWithPred :: forall a
   . DefDec (Dec a)
   => PredZ a
   -> Rec SingleIn '[SelOne a]
selOneWithPred p = E1 (SelOneP p defDec)

updWithPred :: PredZ Int -> Rec SingleIn '[Upd]
updWithPred p = E1 (UpdP p)

-- how do we compose stuff: ie 2 selects one with and one without pred and then override the encoding / decoding etc
-- we need a composable algebra
selSqlWithPred :: forall b a db
   . (DefDec (Dec b), DefEnc (Rec Enc a))
   => PredZ [b]
   -> Text
   -> Sql db a '[Sel b]
selSqlWithPred p = Sql "selSqlWithPred" defEnc (E1 (SelP p defDec))

selOneSqlWithPred :: forall b a db
   . (DefDec (Dec b), DefEnc (Rec Enc a))
   => PredZ b
   -> Text
   -> Sql db a '[SelOne b]
selOneSqlWithPred p = Sql "selOneSqlWithPred" defEnc (E1 (SelOneP p defDec))

selSql :: (DefDec (Dec b), DefEnc (Rec Enc a))
   => Text
   -> Sql db a '[Sel b]
selSql = mkSql "selSql"

selSqlF :: (DefDec (Dec (F rs)), DefEnc (Rec Enc a))
   => Text
   -> Sql db a '[Sel (F rs)]
selSqlF = mkSql "selSqlF"

selOneSql :: (DefDec (Dec b), DefEnc (Rec Enc a))
   => Text
   -> Sql db a '[SelOne b]
selOneSql = mkSql "selOneSql"

selOneSqlF :: (DefDec (Dec (F rs)), DefEnc (Rec Enc a))
   => Text
   -> Sql db a '[SelOne (F rs)]
selOneSqlF = mkSql "selOneSqlF"

instance DefDec (SingleIn (UpdN op val)) where
  defDec = UpdNP
instance DefDec (SingleIn Upd) where
  defDec = UpdP ptrue
instance DefDec (SingleIn SelRaw) where
  defDec = SelRawP ptrue
instance DefDec (Dec a) => DefDec (SingleIn (Sel a)) where
  defDec = SelP ptrue defDec
instance DefDec (Dec a) => DefDec (SingleIn (SelOne a)) where
  defDec = SelOneP ptrue defDec

instance DefDec (SingleIn a) => DefDec (SingleIn (Alle a)) where
  defDec = AlleP defDec ptrue
instance DefDec (SingleIn a) => DefDec (SingleIn (Some rev n a)) where
  defDec = SomeP defDec ptrue
instance (DefDec (SingleIn a), DefDec (SingleIn b)) => DefDec (SingleIn (a :+: b)) where
  defDec = defDec :+: defDec

-- | predicate (==0) for an update result set
type family U0 where U0 = UpdN 'OPEQ 0
-- | predicate (==1) for an update result set
type family U1 where U1 = UpdN 'OPEQ 1
-- | predicate (>0) for an update result set
type family UGT0 where UGT0 = UpdN 'OPGT 0

-- | Allows user to provide a equality predicate at the type level for an update result set
type family UEQ (n :: Nat) = (w :: Type) | w -> n where
  UEQ n = UpdN 'OPEQ n

-- | Allows user to provide a "greater then or equal to" predicate at the type level for an update result set
type family UGE (n :: Nat) = (w :: Type) | w -> n where
  UGE n = UpdN 'OPGE n

-- | Allows user to provide a "greater then" predicate at the type level for an update result set
type family UGT (n :: Nat) = (w :: Type) | w -> n where
  UGT n = UpdN 'OPGT n

-- | Allows user to provide a "less then or equal to" predicate at the type level for an update result set
type family ULE (n :: Nat) = (w :: Type) | w -> n where
  ULE n = UpdN 'OPLE n

-- | Allows user to provide a "less then" predicate at the type level for an update result set
type family ULT (n :: Nat) = (w :: Type) | w -> n where
  ULT n = UpdN 'OPLT n

-- try to use UpdN instead: we still need these runtime versions as we cannot combine U0 and U1
u0 :: SingleIn Upd
u0 = UpdP (peq 0)

u1 :: SingleIn Upd
u1 = UpdP (peq 1)

ugt :: Int -> SingleIn Upd
ugt = UpdP . pgt

ugt0 :: SingleIn Upd
ugt0 = UpdP (pgt 0)

ueq :: Int -> SingleIn Upd
ueq = UpdP . peq

-- | generates n sql input placeholders (used for sql inserts)
qqsn :: Int -> Text
qqsn = vvs . flip replicate "?"

-- | generates length n sql input placeholders (used for sql inserts)
qqs :: [a] -> Text
qqs = qqsn . length

-- | concatenates column names together used for sql inserts
vvs :: [Text] -> Text
vvs xs = "(" <> T.intercalate "," xs <> ")"

-- | generates m by n sql input placeholders (used for sql inserts for multiple rows at a time)
qqrc :: (Int, Int) -> Text
qqrc (r,c) = T.intercalate ", " (replicate r (qqsn c))

-- | determines if a database is writeable or not
type family WriteableDB (arg :: db) :: Bool

{-
type family AssertMsg (tf :: Bool) (msg :: Symbol) :: Constraint where
  AssertMsg 'True msg = ()
  AssertMsg 'False msg = GL.TypeError ('GL.Text msg)
-}

-- | determines if the resultsets require update access or not
type family WriteableRS (rs :: [Type]) :: Bool where
  WriteableRS rs = P.SUnWrap (P.FoldMap (P.SAllSym0 :.: WriteableOneSym0) rs)

type family WriteableOne (r :: Type) :: Bool where
  WriteableOne Upd = 'True
  WriteableOne (UpdN _ _) = 'True
  WriteableOne (Sel x) = 'False
  WriteableOne (SelOne x) = 'False
  WriteableOne SelRaw = 'False
  WriteableOne (Alle a) = WriteableOne a
  WriteableOne (Some rev n a) = WriteableOne a
  WriteableOne (a :+: b) = WriteableOne a P.|| WriteableOne b
  WriteableOne o = GL.TypeError ('GL.Text "WriteableOne: programmer error: unhandled type o=" ':<>: 'GL.ShowType o)

data WriteableOneSym0 :: Type ~> Bool
type instance P.Apply WriteableOneSym0 x = WriteableOne x

-- | 'ChkLast' fails if you have Alle as the non last element
type family ChkLast w :: Bool where
  ChkLast (Alle a) = GL.TypeError ('GL.Text "Alle has to be the last in the HList"
                             ':$$: 'GL.Text "It doesnt make sense to have stuff after it as Alle consumes everything"
                                 )
  ChkLast a = 'True

data ChkLastSym0 :: Type ~> Bool
type instance P.Apply ChkLastSym0 x = ChkLast x

type ValidateNested rs = (MultiLast rs ~ 'True, ValidNestAll rs ~ 'True)

{-
type family MultiLast (rs :: [Type]) :: Bool where
  MultiLast '[] = 'True
  MultiLast (r ': s ': rs) = ChkLast r P.&& MultiLast (s ': rs)
  MultiLast (r ': rs) = MultiLast rs
-}
-- | 'MultiLast' checks that if Alle is present then it is the last entry only
type family MultiLast (rs :: [Type]) :: Bool where
  MultiLast rs = P.Maybe' 'True (P.All'Sym1 ChkLastSym0 :.: P.FstSym0) (P.UnSnoc rs)

data ValidNestSym0 :: Type ~> Bool
type instance Apply ValidNestSym0 x = ValidNest x

type family ValidNestAll (rs :: [Type]) :: Bool where
  ValidNestAll rs = P.SUnWrap (P.FoldMap (P.SAllSym0 :.: ValidNestSym0) rs)

-- | 'ValidNest' checks to see that there are no nested Alle :+: or Some
type family ValidNest (r :: Type) :: Bool where
  ValidNest (Alle a) = ValidNest1 a
  ValidNest (Some rev n a) = ValidNest1 a
  ValidNest (a :+: b) = ValidNest1 a P.&& ValidNest1 b
  ValidNest a = 'True

type family ValidNest1 (w :: Type) :: Bool where
  ValidNest1 (Alle a) = GL.TypeError ('GL.Text "Alle is nested within another construct"
                            ':$$: 'GL.Text "Doesnt make sense cos either Single or Multiple but not a Single(Multiple) or Multiple(Multiple)"
                                 )
  ValidNest1 (Some rev n a) = GL.TypeError ('GL.Text "Some is nested within another construct"
                            ':$$: 'GL.Text "Doesnt make sense cos either Single or Multiple but not a Single(Multiple) or Multiple(Multiple)"
                                 )
  ValidNest1 (a :+: b) = ValidNest1 a P.&& ValidNest1 b
  ValidNest1 a = 'True

-- eg hasError @SingleColE
-- | 'hasError' checks for errors of a given type p (use typeapplications)
hasError :: forall p a . NatToInt (RIndex p SE') => Either SE a -> Bool
hasError = not . null . getErrors @p

-- | 'getErrors' returns list of errors of given type p (use typeapplications)
getErrors :: forall p a . NatToInt (RIndex p SE') => Either SE a -> [p]
getErrors = either (xes @p) (const [])

