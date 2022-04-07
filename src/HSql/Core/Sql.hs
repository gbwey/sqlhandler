{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
 Module      : HSql.Core.Sql
 Description : pure functions for describing, prepocessing and postprocessing Sql
 Copyright   : (c) Grant Weyburne, 2016
 License     : BSD-3
-}
module HSql.Core.Sql where

import Control.Arrow
import Control.DeepSeq (NFData)
import Control.Monad
import Control.Monad.State.Strict
import Data.Bifunctor
import Data.Bool
import Data.Coerce
import Data.Kind
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as N
import qualified Data.List.NonEmpty.Extra as NE
import Data.Maybe
import Data.Pos
import Data.Proxy (Proxy (Proxy))
import Data.String (IsString (fromString))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Internal.Builder (fromText)
import Data.These.Combinators
import qualified Data.Type.Bool as BT
import Data.Vinyl
import Data.Vinyl.CoRec (CoRec (..))
import qualified Data.Vinyl.Core as V
import qualified Data.Vinyl.Functor as V
import qualified Data.Vinyl.Recursive as VR
import Data.Vinyl.TypeLevel hiding (Nat)
import Database.HDBC (SqlValue (..))
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import GHC.TypeLits (ErrorMessage ((:$$:), (:<>:)), KnownNat, Nat)
import qualified GHC.TypeLits as GL
import HSql.Core.Common
import HSql.Core.Decoder
import HSql.Core.Encoder
import HSql.Core.ErrorHandler
import HSql.Core.One
import HSql.Core.Operator
import Primus.Error
import Primus.Fold
import Primus.List
import qualified Primus.TypeLevel as TP
import Text.Shakespeare.Text (ToText (toText))

{- | 'Sql' is the core ADT that holds a vinyl record of encoders for the input
 and a vinyl record of decoders for the ouput and then the Sql text
-}
data Sql db a b = -- | sql
  Sql
  { sDescription :: !Text
  -- ^ description used for logging and error handling
  , sEncoders :: !(Rec Enc a)
  -- ^ a record of encoders matching the input parameters
  , sDecoders :: !(Rec SingleIn b)
  -- ^ a record of decoders matching the output columns
  , sSql :: !Text
  }
  deriving stock (Generic)

-- | constraints for the input encoders
type RSqlInputC a = (ReifyConstraint Show Enc a, RMap a, RecordToList a)

-- | constraints for the output decoders
type RSqlOutputC b = (ReifyConstraint Show SingleIn b, RMap b, RecordToList b)

deriving stock instance
  ( RSqlInputC a
  , RSqlOutputC b
  ) =>
  Show (Sql db a b)

instance ToText (Sql db a b) where
  toText = fromText . sSql

-- | 'UpdN' is similar to 'Upd' but encodes a type level predicate on the return code
newtype UpdN (op :: Op) = UpdN Int
  deriving stock (Show, Eq, Generic)
  deriving newtype (Num, Ord)

instance NFData (UpdN op)

-- | 'Upd' holds a return code from a sql update call
newtype Upd = Upd Int
  deriving stock (Show, Eq, Generic)
  deriving newtype (Num, Ord)

instance NFData Upd

-- | 'SelRowCol' holds a single row and a single column of data
data SelRowCol a = SelRowCol !a !RMeta
  deriving stock (Show, Eq, Generic, Functor)

instance NFData a => NFData (SelRowCol a)

-- | 'SelCol' holds a single column of data
data SelCol a = SelCol ![a] !RMeta
  deriving stock (Show, Eq, Generic, Functor)

instance NFData a => NFData (SelCol a)

instance Semigroup (RState (SelCol a)) where
  RState{rsIn = w, rsOut = SelCol xs meta}
    <> RState{rsOut = SelCol xs' _} =
      RState{rsIn = w, rsOut = SelCol (xs <> xs') meta}

-- | 'SelRow' holds a single row of data
data SelRow a = SelRow !a !RMeta
  deriving stock (Show, Eq, Generic, Functor)

instance NFData a => NFData (SelRow a)

-- | 'Sel' holds a zero or more rows of data
data Sel a = Sel ![a] !RMeta
  deriving stock (Show, Eq, Generic, Functor)

instance NFData a => NFData (Sel a)

instance Semigroup (RState (Sel a)) where
  RState{rsIn = w, rsOut = Sel xss meta}
    <> RState{rsOut = Sel xss' _} =
      RState{rsIn = w, rsOut = Sel (xss <> xss') meta}

-- | 'Rev' is a higher order function that reverses the resultsets order and runs the stuff inside then reverses the resulting resultsets
newtype Rev a = Rev a
  deriving stock (Show, Eq, Generic, Functor)

instance NFData a => NFData (Rev a)

-- | 'Alle' is a higher order function that holds zero or more resultsets of the same type
newtype Alle a = Alle [a]
  deriving stock (Show, Eq, Generic, Functor)

instance NFData a => NFData (Alle a)

-- | 'Some' is a higher order function that holds one or more resultsets of the same type
newtype Some a = Some (NonEmpty a)
  deriving stock (Show, Eq, Generic, Functor)

instance NFData a => NFData (Some a)

-- | 'May' is a higher order function that holds zero or more resultsets of the same type
newtype May a = May (Maybe a)
  deriving stock (Show, Eq, Generic, Functor)

instance NFData a => NFData (May a)

-- | a higher order function that holds a resultset of type a or of type b
newtype a :+: b = Or (Either a b)
  deriving stock (Show, Eq, Generic, Functor)

instance (NFData a, NFData b) => NFData (a :+: b)

instance Bifunctor (:+:) where
  bimap f g (Or lr) = Or $ bimap f g lr

-- | a higher order function that holds both a resultset of type a and of type b
data a :*: b = Both !a !b
  deriving stock (Show, Eq, Generic, Functor)

instance (NFData a, NFData b) => NFData (a :*: b)

instance Bifunctor (:*:) where
  bimap f g (Both a b) = Both (f a) (g b)

-- | 'Range' is a higher order function that is similar to 'Alle' but holds from m to n resultsets of the same type
newtype Range (m :: Nat) (n :: Nat) a = Range [a]
  deriving stock (Show, Eq, Generic, Functor)

instance NFData a => NFData (Range m n a)

-- | 'Exact' is a higher order function that is similar to 'Alle' but holds exactly n resultsets of the same type
newtype Exact (n :: Nat) a = Exact (NonEmpty a)
  deriving stock (Show, Eq, Generic, Functor)

instance NFData a => NFData (Exact n a)

-- | type synonym for any type of resultset
type AnyRaw = Upd :+: SelRaw

-- | 'SelRaw' holds a query resultset with undecoded data
data SelRaw = SelRaw ![[SqlValue]] !RMeta
  deriving stock (Show, Eq, Generic)

instance NFData SelRaw

-- | keeps track of state as we traverse over the resultsets for post processing
data SingleState = SingleState
  { stPos :: !Int
  , stRss :: ![ResultSet]
  , stRev :: !Bool
  }
  deriving stock (Generic, Show, Eq)

instance NFData SingleState

-- | 'RState' holds the input/ouput for a given resultset
data RState a = RState
  { rsIn :: !(SingleIn a)
  , rsOut :: a -- undefined so cannot use strict
  }
  deriving stock (Generic, Show)

deriving stock instance
  (Eq a, Eq (SingleIn a)) =>
  Eq (RState a)

-- | 'toRState' sets up the initial state before processing all the resultsets
toRState :: HasCallStack => Rec SingleIn rs -> Rec RState rs
toRState = VR.rmap $ \xa ->
  RState
    xa
    (programmError "RState: rsOut doesn't have a value!")

-- | 'Single' represents a single result set
type Single :: Type -> Constraint
class Single a where
  -- | input type
  data SingleIn a

  -- | output type
  type SingleOut a

  showF :: SingleIn a -> String
  singleCol ::
    HasCallStack =>
    SingleIn a ->
    StateT SingleState (Either SE) a
  toSingleOut :: a -> SingleOut a
  rsOutUnwrap :: RState a -> SingleOut a
  rsOutUnwrap = toSingleOut . rsOut

instance Single a => Show (SingleIn a) where
  show = showF

-- | add an error using the current state  (skip the first error message)
addError :: String -> SingleState -> Either SE a -> Either SE a
addError = addError' ""

-- | add an error using the current state
addError' :: String -> String -> SingleState -> Either SE a -> Either SE a
addError' emsg msg z = left (CoRec (V.Identity (SqlE msg (stPos z) emsg (stRss z))) N.<|)

{-
type SS a = StateT SingleState (Either SE) a

addError'' :: String -> String -> StateT SingleState (Either SE) a -> StateT SingleState (Either SE) a
addError'' emsg msg ma =
  StateT $ \z -> do
    left (CoRec (V.Identity (SqlE msg (stPos z) emsg (stRss z))) N.<|) $
      runStateT ma z
-}

-- | 'Upd' represents any non select query eg dml insert / update / delete o ddl create / drop / alter
instance Single Upd where
  data SingleIn Upd = UpdP deriving stock (Generic, Eq)
  type SingleOut Upd = Int
  showF UpdP = "Upd"
  toSingleOut (Upd a) = a

  singleCol k@UpdP = StateT $ \z -> do
    let msg = showF k
    case stRss z of
      rs1 : rss' -> addError msg z $ do
        rc <- updImpl rs1
        nextSingleStateExit
          msg
          z
          rss'
          (Upd rc)
      [] -> failNR msg (stPos z) emptyResultSetMessage

-- | 'UpdN' is similar to 'Upd' but adds a type level predicate on the number of rows returned
instance ExprC op => Single (UpdN (op :: Op)) where
  data SingleIn (UpdN op) = UpdNP deriving stock (Generic, Eq)
  type SingleOut (UpdN op) = Int
  toSingleOut (UpdN a) = a
  showF UpdNP = "UpdN " ++ opPretty False (evalC @op Nothing)

  singleCol k@UpdNP = StateT $ \z -> do
    let msg = showF k
    case stRss z of
      rs1 : rss' -> addError msg z $ do
        rc <- updImpl rs1
        case evalC @op (Just rc) of
          op@OpRet{opBool = mb} ->
            case mb of
              Nothing ->
                failBad msg ("pos=" ++ show (stPos z) ++ " update predicate returned nothing! how does this happen " ++ opPretty True op) ""
              Just False ->
                failUpdN msg (stPos z) ("update predicate failed: rc=" ++ show rc ++ " => " ++ opPretty True op)
              Just True -> do
                nextSingleStateExit
                  msg
                  z
                  rss'
                  (UpdN rc)
      [] -> failNR msg (stPos z) emptyResultSetMessage

-- | 'SelCol' represents a select query with one column
instance Single (SelCol a) where
  newtype SingleIn (SelCol a) = SelColP (Dec a) deriving stock (Generic)
  type SingleOut (SelCol a) = [a]
  showF (SelColP _) = "SelCol"
  toSingleOut (SelCol a _) = a

  singleCol k@(SelColP dec) = StateT $ \z -> do
    let msg = showF k
    case stRss z of
      rs1 : rss' -> addError msg z $ do
        chkOneColumn msg rs1
        (meta, a) <- selImpl msg rs1 dec
        nextSingleStateExit
          msg
          z
          rss'
          (SelCol a meta)
      [] -> failNR msg (stPos z) emptyResultSetMessage

-- | 'SelRowCol' is similar to 'Sel' but expects one row and one col
instance Single (SelRowCol a) where
  newtype SingleIn (SelRowCol a) = SelRowColP (Dec a) deriving stock (Generic)
  type SingleOut (SelRowCol a) = a
  showF (SelRowColP _) = "SelRowCol"
  toSingleOut (SelRowCol a _) = a

  singleCol k@(SelRowColP dec) = StateT $ \z -> do
    let msg = showF k
    case stRss z of
      rs1 : rss' -> addError msg z $ do
        chkOneColumn msg rs1
        (meta, xxs) <- selImpl msg rs1 dec
        case xxs of
          [xs] -> do
            nextSingleStateExit
              msg
              z
              rss'
              (SelRowCol xs meta)
          _wrongrowcnt -> failOneRow msg (stPos z) ("expected 1 row but found " ++ show (length xxs)) rs1
      [] -> failNR msg (stPos z) emptyResultSetMessage

-- | 'SelRow' is similar to 'Sel' but expects one row
instance Single (SelRow a) where
  newtype SingleIn (SelRow a) = SelRowP (Dec a) deriving stock (Generic)
  type SingleOut (SelRow a) = a
  showF (SelRowP _) = "SelRow"
  toSingleOut (SelRow a _) = a

  singleCol k@(SelRowP dec) = StateT $ \z -> do
    let msg = showF k
    case stRss z of
      rs1 : rss' -> addError msg z $ do
        (meta, xxs) <- selImpl msg rs1 dec
        case xxs of
          [xs] -> do
            nextSingleStateExit
              msg
              z
              rss'
              (SelRow xs meta)
          _wrongrowcnt -> failOneRow msg (stPos z) ("expected 1 row but found " ++ show (length xxs)) rs1
      [] -> failNR msg (stPos z) emptyResultSetMessage

-- | 'Sel' represents a select query
instance Single (Sel a) where
  newtype SingleIn (Sel a) = SelP (Dec a) deriving stock (Generic)
  type SingleOut (Sel a) = [a]
  showF (SelP _) = "Sel"
  toSingleOut (Sel a _) = a

  singleCol k@(SelP dec) = StateT $ \z -> do
    let msg = showF k
    case stRss z of
      rs1 : rss' -> addError msg z $ do
        (meta, a) <- selImpl msg rs1 dec
        nextSingleStateExit
          msg
          z
          rss'
          (Sel a meta)
      [] -> failNR msg (stPos z) emptyResultSetMessage

deriving stock instance (Eq (SingleIn a)) => Eq (SingleIn (Range m n a))

-- | 'Range' expects to handle between m and n resultsets of type "a"
instance
  ( Single a
  , KnownNat m
  , KnownNat n
  , TP.FailUnless
      ((m GL.+ 1) GL.<=? n)
      ( 'GL.Text "Range m n a: requires m < n but found m="
          ':<>: 'GL.ShowType m
          ':<>: 'GL.Text " and n="
          ':<>: 'GL.ShowType n
          ':$$: 'GL.Text "  a="
          ':<>: 'GL.ShowType a
      )
  ) =>
  Single (Range m n a)
  where
  newtype SingleIn (Range m n a) = RangeP (SingleIn a) deriving stock (Generic)
  type SingleOut (Range m n a) = [SingleOut a]
  toSingleOut (Range as) = map toSingleOut as

  showF (RangeP a) =
    let m = TP.pnat @m
        n = TP.pnat @n
     in "Range[" ++ show m ++ "," ++ show n ++ "] " ++ showF a

  singleCol k@(RangeP one) = StateT $ \z -> do
    let msg = showF k
    let m = TP.pnat @m
        n = TP.pnat @n
    addError' (show (length (stRss z)) ++ " resultsets") msg z $ do
      (sc, z') <-
        foldM
          ( \(sc, zz) i -> do
              (sc', zz') <-
                addError (msg ++ "(" ++ show i ++ ")") zz $ do
                  runStateT (singleCol one) zz
              checkRssThenExit
                (msg ++ "(" ++ show i ++ ")")
                zz
                zz'
                (Alle (coerce sc `snocL` sc'))
          )
          (Alle [], z)
          [1 .. m]
      (sc', z'') <-
        runStateT
          ( handleSomeResultSets
              (Just (n - m))
              msg
              one
              sc
          )
          z'
      pure (coerce sc', z'')

deriving stock instance (Eq (SingleIn a)) => Eq (SingleIn (Rev a))

{- | 'Rev' expect the rest of the resultsets are of type "a"
   this is a very special case:just moves a cursor from one end to the other
   then restores the cursor at the end: so the post condition is a bit weird
   todo: remove checkRss msg z' z'' cos redundant!
-}
instance Single a => Single (Rev a) where
  newtype SingleIn (Rev a) = RevP (SingleIn a) deriving stock (Generic)
  type SingleOut (Rev a) = SingleOut a
  showF (RevP a) = "Rev " ++ showF a
  toSingleOut (Rev a) = toSingleOut a

  singleCol k@(RevP one) = StateT $ \z -> do
    let msg = showF k
    addError' (show (length (stRss z)) ++ " resultsets") msg z $ do
      let nbefore = length (stRss z)
          posbefore =
            if stRev z
              then stPos z - nbefore
              else stPos z + nbefore
      let z' =
            SingleState
              posbefore -- need to adjust the position if fwd vs bwd
              (reverseF (stRss z)) -- reverse resultsets
              (not (stRev z))
      (sc, z'') <- runStateT (singleCol one) z'
      -- we skip checkRss msg z' z'' cos should have be checked lower down in 'a' in eg Exact Both Sel ...
      when _CheckRss $ void $ checkRss msg z' z'' -- this is correct but no need cos handled downstream: Rev a means that 'a' is should have been checked in Sel/Exact ...
      pure (Rev sc, z{stRss = reverseF (stRss z'')})

deriving stock instance (Eq (SingleIn a)) => Eq (SingleIn (Alle a))

-- | 'Alle' expect the rest of the resultsets are of type "a"
instance Single a => Single (Alle a) where
  newtype SingleIn (Alle a) = AlleP (SingleIn a) deriving stock (Generic)
  type SingleOut (Alle a) = [SingleOut a]
  showF (AlleP a) = "Alle " ++ showF a
  toSingleOut (Alle as) = map toSingleOut as

  singleCol k@(AlleP one) = StateT $ \z -> do
    let msg = showF k
    addError' (show (length (stRss z)) ++ " resultsets") msg z $ do
      runStateT
        ( handleSomeResultSets
            Nothing
            msg
            one
            (Alle [])
        )
        z

deriving stock instance (Eq (SingleIn a)) => Eq (SingleIn (Some a))

-- | 'Some' expects at least one of type "a" then the rest of the resultsets are of type "a"
instance Single a => Single (Some a) where
  newtype SingleIn (Some a) = SomeP (SingleIn a) deriving stock (Generic)
  type SingleOut (Some a) = NonEmpty (SingleOut a)
  showF (SomeP a) = "Some " ++ showF a
  toSingleOut (Some as) = N.map toSingleOut as

  singleCol k@(SomeP one) = StateT $ \z -> do
    let msg = showF k
    addError' (show (length (stRss z)) ++ " resultsets") msg z $ do
      (scg, ss) <-
        runStateT
          ( handleSomeResultSets
              Nothing
              msg
              one
              (Alle [])
          )
          z
      case scg of
        Alle [] -> failNR msg (stPos z) "expected at least one resultset"
        Alle (a : as) -> return (Some (a :| as), ss)

deriving stock instance (Eq (SingleIn a)) => Eq (SingleIn (May a))

-- | 'May' expects at most one resultsets of type "a"
instance Single a => Single (May a) where
  newtype SingleIn (May a) = MayP (SingleIn a) deriving stock (Generic)
  type SingleOut (May a) = Maybe (SingleOut a)
  showF (MayP a) = "May " ++ showF a
  toSingleOut (May a) = toSingleOut <$> a

  singleCol k@(MayP one) = StateT $ \z -> do
    let msg = showF k
    addError' (show (length (stRss z)) ++ " resultsets") msg z $ do
      case runStateT (singleCol one) z of
        Left _ ->
          Right (May Nothing, z)
        Right (sc, z') -> do
          checkRssThenExit
            msg
            z
            z'
            (May (Just sc))

deriving stock instance
  (Single a, Single b, Eq (SingleIn a), Eq (SingleIn b)) =>
  Eq (SingleIn (a :+: b))

-- | a sum type on the resultset: ie either type "a" or type "b"
instance (Single a, Single b) => Single (a :+: b) where
  data SingleIn (a :+: b) = SingleIn a :+: SingleIn b deriving stock (Generic)
  type SingleOut (a :+: b) = Either (SingleOut a) (SingleOut b)
  showF (a :+: b) = showF a ++ " :+: " ++ showF b
  toSingleOut (Or lr) = either (Left . toSingleOut) (Right . toSingleOut) lr

  -- try the first: if it fails try the second using the same initial resultsets as input
  singleCol k@(a :+: b) = StateT $ \z -> do
    let msg = showF k
        msg' rgt = msg ++ "(" ++ bool "lhs" "rhs" rgt ++ ")"
    addError msg z $ do
      (rgt, (sc, z')) <- case runStateT (singleCol a) z of
        Left e1 ->
          case runStateT (singleCol b) z of
            Left e2 -> addError' "failed trying Left and Right" (msg' False) z $ Left (e1 <> e2)
            Right (sc, z') -> Right (False, (Or (Right sc), z'))
        Right (sc, z') -> Right (True, (Or (Left sc), z'))
      checkRssThenExit
        (msg' rgt)
        z
        z'
        sc

deriving stock instance
  (Single a, Single b, Eq (SingleIn a), Eq (SingleIn b)) =>
  Eq (SingleIn (a :*: b))

-- | a product type on the resultset: type "a" and type "b"
instance (Single a, Single b) => Single (a :*: b) where
  data SingleIn (a :*: b) = SingleIn a :*: SingleIn b deriving stock (Generic)
  type SingleOut (a :*: b) = (SingleOut a, SingleOut b)
  showF (a :*: b) = showF a ++ " :*: " ++ showF b
  toSingleOut (Both a b) = (toSingleOut a, toSingleOut b)
  singleCol k@(a :*: b) = StateT $ \z -> do
    let msg rgt = showF k ++ "(" ++ bool "lhs" "rhs" rgt ++ ")"
    (sc1, z') <- addError (msg False) z $ do
      (sc1, z') <- runStateT (singleCol a) z
      checkRssThenExit
        (msg False)
        z
        z'
        sc1

    addError (msg True) z' $ do
      (sc2, z'') <- runStateT (singleCol b) z'
      checkRssThenExit
        (msg True)
        z'
        z''
        (Both sc1 sc2)

deriving stock instance Eq (SingleIn a) => Eq (SingleIn (Exact n a))

-- | represents exactly "n" resultsets of type "a"
instance
  ( Single a
  , KnownNat n
  , TP.FailUnless
      (1 GL.<=? n)
      ( 'GL.Text "Exact n a: requires n >= 1 but found n="
          ':<>: 'GL.ShowType n
          ':$$: 'GL.Text "  a="
          ':<>: 'GL.ShowType a
      )
  ) =>
  Single (Exact (n :: Nat) a)
  where
  newtype SingleIn (Exact n a) = ExactP (SingleIn a) deriving stock (Generic)
  type SingleOut (Exact n a) = NonEmpty (SingleOut a)
  showF (ExactP a) = "Exact[" ++ show (TP.pnat @n) ++ "] " ++ showF a
  toSingleOut (Exact xs) = N.map toSingleOut xs
  singleCol k@(ExactP one) = StateT $ \z -> do
    -- remember i and i' are just for bookkeeping
    -- how much is used is determined by rss before and after
    -- pos+n should probably be pos + length rss - length rssout
    let msg = showF k
    let n = TP.pnat @n
    -- use a list then convert to nonempty is easiest and less code repetition
    -- tricky cos cant start with empty list cos nonempty
    -- CORRECT BY CONSTRUCTION
    (sc0, z0) <- addError (msg ++ "(1)") z $ do
      (sc0, z0) <- runStateT (singleCol one) z
      checkRssThenExit
        (msg ++ " i=1")
        z
        z0
        (Exact (pure sc0))

    foldM
      ( \(sc, zz) i -> do
          -- left msg singleCol so we can add context of which iteration in this Exact loop
          -- ie add context exception if fails
          (sc', zz') <-
            addError (msg ++ " i=" ++ show i) zz $ do
              runStateT (singleCol one) zz
          checkRssThenExit
            (msg ++ " i=" ++ show i)
            zz
            zz'
            (coerce $ coerce sc NE.|> sc')
      )
      (sc0, z0)
      [2 .. n]

-- | represents an untyped query resultset
instance Single SelRaw where
  data SingleIn SelRaw = SelRawP deriving stock (Generic, Eq)
  type SingleOut SelRaw = [[SqlValue]]
  showF SelRawP = "SelRaw"
  toSingleOut (SelRaw xs _) = xs

  singleCol k@SelRawP = StateT $ \z -> do
    let msg = showF k
    case stRss z of
      rs1 : rss' -> addError msg z $ do
        (meta, a) <- selRawImpl rs1
        nextSingleStateExit
          msg
          z
          rss'
          (SelRaw a meta)
      [] -> failNR msg (stPos z) emptyResultSetMessage

-- | parses the resultsets into the dsl slots
processRetCol ::
  ( RecAll RState rs SingleZ
  , ( ValidateNested rs
    , HasCallStack
    )
  ) =>
  Rec SingleIn rs ->
  [ResultSet] ->
  Either SE (Rec RState rs)
processRetCol !decRec !rss = do
  (w, z) <-
    flip runStateT (SingleState 0 rss False) $
      rtraverse
        (\(V.Compose (V.Dict x)) -> singleColZ x)
        (VR.reifyConstraint (Proxy @SingleZ) (toRState decRec))
  case stRss z of
    [] -> Right w
    _ : _ -> failUncRS "processRetCol'" (stPos z) "Unconsumed" (stRss z)

{- | 'SingleZ' is a simple state monad wrapper around 'Single' which processes a single 'RState' entry
 we need this wrapper as vinyl uses Rec RState rs wraps RState around each rs
-}
type SingleZ :: Type -> Constraint
class SingleZ a where
  singleColZ ::
    HasCallStack =>
    a ->
    StateT SingleState (Either SE) a

-- only one instance that wraps 'Single' which is needed for 'processRet' and 'processRetCol'
instance Single a => SingleZ (RState a) where
  singleColZ rstate = do
    sc <- singleCol (rsIn rstate)
    pure
      rstate
        { rsOut = sc
        }

-- | handles a single resultset
nextSingleStateExit ::
  String ->
  SingleState ->
  [ResultSet] ->
  a ->
  Either SE (a, SingleState)
nextSingleStateExit msg z rss sc = do
  let z' =
        z
          { stPos = if stRev z then stPos z - 1 else stPos z + 1
          , stRss = rss
          }
  when _CheckRss $ void $ checkRss msg z z'
  pure (sc, z')

-- | check that state is valid and if ok then return
checkRssThenExit ::
  String ->
  SingleState ->
  SingleState ->
  a ->
  Either SE (a, SingleState)
checkRssThenExit msg z z' sc = do
  when _CheckRss $ void $ checkRss msg z z'
  pure (sc, z')

-- | convert up to n resultsets to tuples
type SingleOuts :: [Type] -> Type
type family SingleOuts rs where
  SingleOuts '[] = ()
  SingleOuts '[a] = SingleOut a
  SingleOuts '[a, b] = (SingleOut a, SingleOut b)
  SingleOuts '[a, b, c] = (SingleOut a, SingleOut b, SingleOut c)
  SingleOuts '[a, b, c, d] = (SingleOut a, SingleOut b, SingleOut c, SingleOut d)
  SingleOuts '[a, b, c, d, e] = (SingleOut a, SingleOut b, SingleOut c, SingleOut d, SingleOut e)
  SingleOuts '[a, b, c, d, e, f] = (SingleOut a, SingleOut b, SingleOut c, SingleOut d, SingleOut e, SingleOut f)
  SingleOuts '[a, b, c, d, e, f, g] = (SingleOut a, SingleOut b, SingleOut c, SingleOut d, SingleOut e, SingleOut f, SingleOut g)
  SingleOuts '[a, b, c, d, e, f, g, h] = (SingleOut a, SingleOut b, SingleOut c, SingleOut d, SingleOut e, SingleOut f, SingleOut g, SingleOut h)
  SingleOuts '[a, b, c, d, e, f, g, h, i] = (SingleOut a, SingleOut b, SingleOut c, SingleOut d, SingleOut e, SingleOut f, SingleOut g, SingleOut h, SingleOut i)
  SingleOuts '[a, b, c, d, e, f, g, h, i, j] = (SingleOut a, SingleOut b, SingleOut c, SingleOut d, SingleOut e, SingleOut f, SingleOut g, SingleOut h, SingleOut i, SingleOut j)

-- | convert Rec RState rs to tuple
type PGen :: [Type] -> Constraint
class PGen rs where
  ext :: Rec RState rs -> SingleOuts rs

instance PGen '[] where
  ext RNil = ()

instance Single a => PGen '[a] where
  ext (r1 :& RNil) = rsOutUnwrap r1

instance (Single a, Single b) => PGen '[a, b] where
  ext (r1 :& r2 :& RNil) = (rsOutUnwrap r1, rsOutUnwrap r2)

instance (Single a, Single b, Single c) => PGen '[a, b, c] where
  ext (r1 :& r2 :& r3 :& RNil) = (rsOutUnwrap r1, rsOutUnwrap r2, rsOutUnwrap r3)

instance (Single a, Single b, Single c, Single d) => PGen '[a, b, c, d] where
  ext (r1 :& r2 :& r3 :& r4 :& RNil) = (rsOutUnwrap r1, rsOutUnwrap r2, rsOutUnwrap r3, rsOutUnwrap r4)

instance (Single a, Single b, Single c, Single d, Single e) => PGen '[a, b, c, d, e] where
  ext (r1 :& r2 :& r3 :& r4 :& r5 :& RNil) = (rsOutUnwrap r1, rsOutUnwrap r2, rsOutUnwrap r3, rsOutUnwrap r4, rsOutUnwrap r5)

instance (Single a, Single b, Single c, Single d, Single e, Single f) => PGen '[a, b, c, d, e, f] where
  ext (r1 :& r2 :& r3 :& r4 :& r5 :& r6 :& RNil) = (rsOutUnwrap r1, rsOutUnwrap r2, rsOutUnwrap r3, rsOutUnwrap r4, rsOutUnwrap r5, rsOutUnwrap r6)

instance (Single a, Single b, Single c, Single d, Single e, Single f, Single g) => PGen '[a, b, c, d, e, f, g] where
  ext (r1 :& r2 :& r3 :& r4 :& r5 :& r6 :& r7 :& RNil) = (rsOutUnwrap r1, rsOutUnwrap r2, rsOutUnwrap r3, rsOutUnwrap r4, rsOutUnwrap r5, rsOutUnwrap r6, rsOutUnwrap r7)

instance (Single a, Single b, Single c, Single d, Single e, Single f, Single g, Single h) => PGen '[a, b, c, d, e, f, g, h] where
  ext (r1 :& r2 :& r3 :& r4 :& r5 :& r6 :& r7 :& r8 :& RNil) = (rsOutUnwrap r1, rsOutUnwrap r2, rsOutUnwrap r3, rsOutUnwrap r4, rsOutUnwrap r5, rsOutUnwrap r6, rsOutUnwrap r7, rsOutUnwrap r8)

instance (Single a, Single b, Single c, Single d, Single e, Single f, Single g, Single h, Single i) => PGen '[a, b, c, d, e, f, g, h, i] where
  ext (r1 :& r2 :& r3 :& r4 :& r5 :& r6 :& r7 :& r8 :& r9 :& RNil) = (rsOutUnwrap r1, rsOutUnwrap r2, rsOutUnwrap r3, rsOutUnwrap r4, rsOutUnwrap r5, rsOutUnwrap r6, rsOutUnwrap r7, rsOutUnwrap r8, rsOutUnwrap r9)

instance (Single a, Single b, Single c, Single d, Single e, Single f, Single g, Single h, Single i, Single j) => PGen '[a, b, c, d, e, f, g, h, i, j] where
  ext (r1 :& r2 :& r3 :& r4 :& r5 :& r6 :& r7 :& r8 :& r9 :& r10 :& RNil) = (rsOutUnwrap r1, rsOutUnwrap r2, rsOutUnwrap r3, rsOutUnwrap r4, rsOutUnwrap r5, rsOutUnwrap r6, rsOutUnwrap r7, rsOutUnwrap r8, rsOutUnwrap r9, rsOutUnwrap r10)

-- | type family for inductive tuples types
type SingleOuts' :: [Type] -> Type
type family SingleOuts' rs where
  SingleOuts' '[] = ()
  SingleOuts' (a ': as) = (SingleOut a, SingleOuts' as)

-- | alternative approach of converting vinyl record of results to a nested tuple
type PGen' :: [Type] -> Constraint
class PGen' rs where
  ext' :: HasCallStack => Rec RState rs -> SingleOuts' rs

instance PGen' '[] where
  ext' RNil = ()

instance (Single a, PGen' as) => PGen' (a ': as) where
  ext' (r :& rs) = (rsOutUnwrap r, ext' rs)

-- | checks that there is exactly one column in the resultset
chkOneColumn ::
  String ->
  Either Int (RMeta, [[SqlValue]]) ->
  Either SE ()
chkOneColumn msg =
  \case
    z@(Right (_meta, xss)) ->
      case filter ((/= 1) . length . snd) (zip [1 :: Int ..] xss) of
        [] -> pure ()
        (i, xs) : _ ->
          failOneCol msg i ("expected one column only but found " ++ show (length xs) ++ " row=" ++ show i ++ " " ++ show xs) z
    Left _rc -> pure () -- let downstream handle this

-- | handler for a select* style resultsets
selImpl ::
  HasCallStack =>
  String ->
  ResultSet ->
  Dec a ->
  Either SE (RMeta, [a])
selImpl msg z@(Right (meta, xxs)) (Dec dec) = do
  ys <- forM (zip [1 :: Int ..] xxs) $ \(r, xs) -> do
    case dec xs of
      Left es ->
        -- change the code so only DecodingE happens?
        case justThere $ getDecErrors es of -- ConvE can appear as an exception but seemingly always with DecodingE : see liftCE
          Nothing -> normalError $ msg ++ ":selImpl: missing DecodingE" -- cant seem to make this error fire as always has an accompanying DecodingE exception
          Just (ss N.:| _) ->
            let c = length xs - length (deSqlValues ss)
             in Left $ liftDE $ decAddError' msg ("selImpl: row/col " ++ show (r, c)) "" [] es
      Right a -> pure a
  ret <- forM (zip [1 :: Int ..] ys) $ \(i, (a, lft)) -> do
    unless (null lft) $ failUncCol msg i ("selImpl: didnt consume all the columns! row " ++ show i ++ ":leftovers=" ++ show lft) z
    pure a
  pure (meta, ret)
selImpl msg z _ = failURST (msg ++ "(selImpl)") "expected a select but found an update" z

-- | 'updImpl' processes an Update resultset and runs the predicate
updImpl :: ResultSet -> Either SE Int
updImpl (Left i) = pure i
updImpl z@Right{} = failURST "updImpl(Update)" "expected an update but found a select" z

-- | 'selRawImpl' processes a Query resultset and runs the predicate
selRawImpl :: ResultSet -> Either SE (RMeta, [[SqlValue]])
selRawImpl (Right (meta, xs)) = pure (meta, xs)
selRawImpl z@Left{} = failURST "selRawImpl(Select)" "expected a select but found an update" z

-- | 'IsString' instance for 'Sql'
instance (DefEnc (Rec Enc a), DefDec (Rec SingleIn b)) => IsString (Sql db a b) where
  fromString s = mkSql "<fromString>" (T.pack s)

-- | unsafely convert from 'Sql' to another
unsafeCoerceSql :: Text -> Rec Enc a -> Rec SingleIn b -> Sql db1 ax bx -> Sql db2 a b
unsafeCoerceSql str e d s = Sql (str <> "[" <> sDescription s <> "]:coerced") e d (sSql s)

-- | given a number of rows the user needs to calculate the effective number of fields needed ie rows * cols
type ISql db a b = Pos -> (Sql db a b, Pos)

{- | 'ToTuple' converts Rec V.Identity to tuples
   single Tuple is a special case.
   pulls Rec L.Identity (r1,r2,...) into (r1,r2,...)
-}
type ToTuple :: Type -> Type -> Constraint
class ToTuple a b | a -> b, b -> a where
  fromRec :: b -> a
  toRec :: a -> b

instance ToTuple () (Rec V.Identity '[]) where
  fromRec RNil = ()
  toRec () = RNil

instance ToTuple (One a1) (Rec V.Identity '[a1]) where
  fromRec (V.Identity a1 :& RNil) = One a1
  toRec (One a1) = V.Identity a1 :& RNil

instance ToTuple (a1, a2) (Rec V.Identity '[a1, a2]) where
  fromRec (V.Identity a1 :& V.Identity a2 :& RNil) = (a1, a2)
  toRec (a1, a2) = V.Identity a1 :& V.Identity a2 :& RNil

instance ToTuple (a1, a2, a3) (Rec V.Identity '[a1, a2, a3]) where
  fromRec (V.Identity a1 :& V.Identity a2 :& V.Identity a3 :& RNil) = (a1, a2, a3)
  toRec (a1, a2, a3) = V.Identity a1 :& V.Identity a2 :& V.Identity a3 :& RNil

instance ToTuple (a1, a2, a3, a4) (Rec V.Identity '[a1, a2, a3, a4]) where
  fromRec (V.Identity a1 :& V.Identity a2 :& V.Identity a3 :& V.Identity a4 :& RNil) = (a1, a2, a3, a4)
  toRec (a1, a2, a3, a4) = V.Identity a1 :& V.Identity a2 :& V.Identity a3 :& V.Identity a4 :& RNil

instance ToTuple (a1, a2, a3, a4, a5) (Rec V.Identity '[a1, a2, a3, a4, a5]) where
  fromRec (V.Identity a1 :& V.Identity a2 :& V.Identity a3 :& V.Identity a4 :& V.Identity a5 :& RNil) = (a1, a2, a3, a4, a5)
  toRec (a1, a2, a3, a4, a5) = V.Identity a1 :& V.Identity a2 :& V.Identity a3 :& V.Identity a4 :& V.Identity a5 :& RNil

instance ToTuple (a1, a2, a3, a4, a5, a6) (Rec V.Identity '[a1, a2, a3, a4, a5, a6]) where
  fromRec (V.Identity a1 :& V.Identity a2 :& V.Identity a3 :& V.Identity a4 :& V.Identity a5 :& V.Identity a6 :& RNil) = (a1, a2, a3, a4, a5, a6)
  toRec (a1, a2, a3, a4, a5, a6) = V.Identity a1 :& V.Identity a2 :& V.Identity a3 :& V.Identity a4 :& V.Identity a5 :& V.Identity a6 :& RNil

instance ToTuple (a1, a2, a3, a4, a5, a6, a7) (Rec V.Identity '[a1, a2, a3, a4, a5, a6, a7]) where
  fromRec (V.Identity a1 :& V.Identity a2 :& V.Identity a3 :& V.Identity a4 :& V.Identity a5 :& V.Identity a6 :& V.Identity a7 :& RNil) = (a1, a2, a3, a4, a5, a6, a7)
  toRec (a1, a2, a3, a4, a5, a6, a7) = V.Identity a1 :& V.Identity a2 :& V.Identity a3 :& V.Identity a4 :& V.Identity a5 :& V.Identity a6 :& V.Identity a7 :& RNil

instance ToTuple (a1, a2, a3, a4, a5, a6, a7, a8) (Rec V.Identity '[a1, a2, a3, a4, a5, a6, a7, a8]) where
  fromRec (V.Identity a1 :& V.Identity a2 :& V.Identity a3 :& V.Identity a4 :& V.Identity a5 :& V.Identity a6 :& V.Identity a7 :& V.Identity a8 :& RNil) = (a1, a2, a3, a4, a5, a6, a7, a8)
  toRec (a1, a2, a3, a4, a5, a6, a7, a8) = V.Identity a1 :& V.Identity a2 :& V.Identity a3 :& V.Identity a4 :& V.Identity a5 :& V.Identity a6 :& V.Identity a7 :& V.Identity a8 :& RNil

instance ToTuple (a1, a2, a3, a4, a5, a6, a7, a8, a9) (Rec V.Identity '[a1, a2, a3, a4, a5, a6, a7, a8, a9]) where
  fromRec (V.Identity a1 :& V.Identity a2 :& V.Identity a3 :& V.Identity a4 :& V.Identity a5 :& V.Identity a6 :& V.Identity a7 :& V.Identity a8 :& V.Identity a9 :& RNil) = (a1, a2, a3, a4, a5, a6, a7, a8, a9)
  toRec (a1, a2, a3, a4, a5, a6, a7, a8, a9) = V.Identity a1 :& V.Identity a2 :& V.Identity a3 :& V.Identity a4 :& V.Identity a5 :& V.Identity a6 :& V.Identity a7 :& V.Identity a8 :& V.Identity a9 :& RNil

instance ToTuple (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) (Rec V.Identity '[a1, a2, a3, a4, a5, a6, a7, a8, a9, a10]) where
  fromRec (V.Identity a1 :& V.Identity a2 :& V.Identity a3 :& V.Identity a4 :& V.Identity a5 :& V.Identity a6 :& V.Identity a7 :& V.Identity a8 :& V.Identity a9 :& V.Identity a10 :& RNil) = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
  toRec (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = V.Identity a1 :& V.Identity a2 :& V.Identity a3 :& V.Identity a4 :& V.Identity a5 :& V.Identity a6 :& V.Identity a7 :& V.Identity a8 :& V.Identity a9 :& V.Identity a10 :& RNil

-- adds a delimiter ; cos required by postgres (only mssql and postgres support multiple resultsets)

-- | combines two queries as separate resultsets
sqlCombine :: Sql db a1 b1 -> Sql db a2 b2 -> Sql db (a1 ++ a2) (b1 ++ b2)
sqlCombine (Sql desc1 a1 b1 q1) (Sql desc2 a2 b2 q2) =
  Sql ("[" <> desc1 <> "][" <> desc2 <> "]") (rappend a1 a2) (rappend b1 b2) (q1 <> ";\n" <> q2)

-- [*] fails so use [Type]  -- Type == * (* and Type are in Data.Kind)
-- flipped around so DefDec is first: runSql sets the defEnc so defDec is more important
-- runSql usually determines DefEnc so we only need specify DefDec
-- explicit signatures so we can move 'k' away from from first 2 positions
-- not true: k doesnt matter

-- | helper method for defining Sql but using defaults for encoding and decoding
mkSql' :: -- forall (b :: [Type]) (a :: [Type]) k (db :: k).
  (DefDec (Rec SingleIn b), DefEnc (Rec Enc a)) =>
  Text ->
  Sql db a b
mkSql' = mkSql "sqlDef"

-- | helper method for defining Sql but using defaults for encoding and decoding
mkSql :: -- forall (b :: [Type]) (a :: [Type]) k (db :: k).
  (DefDec (Rec SingleIn b), DefEnc (Rec Enc a)) =>
  Text ->
  Text ->
  Sql db a b
mkSql s = Sql s defEnc defDec

-- | decoder instance for Upd
instance DefDec (SingleIn Upd) where
  defDec = UpdP

-- | decoder instance for UpdN
instance DefDec (SingleIn (UpdN op)) where
  defDec = UpdNP

instance DefDec (SingleIn SelRaw) where
  defDec = SelRawP

instance DefDec (Dec a) => DefDec (SingleIn (Sel a)) where
  defDec = SelP defDec

instance DefDec (Dec a) => DefDec (SingleIn (SelRow a)) where
  defDec = SelRowP defDec

instance DefDec (Dec a) => DefDec (SingleIn (SelRowCol a)) where
  defDec = SelRowColP defDec

instance DefDec (Dec a) => DefDec (SingleIn (SelCol a)) where
  defDec = SelColP defDec

instance DefDec (SingleIn a) => DefDec (SingleIn (Rev a)) where
  defDec = RevP defDec

instance DefDec (SingleIn a) => DefDec (SingleIn (Alle a)) where
  defDec = AlleP defDec

instance DefDec (SingleIn a) => DefDec (SingleIn (Some a)) where
  defDec = SomeP defDec

instance DefDec (SingleIn a) => DefDec (SingleIn (Range m n a)) where
  defDec = RangeP defDec

instance DefDec (SingleIn a) => DefDec (SingleIn (Exact n a)) where
  defDec = ExactP defDec

instance (DefDec (SingleIn a), DefDec (SingleIn b)) => DefDec (SingleIn (a :+: b)) where
  defDec = defDec :+: defDec

instance (DefDec (SingleIn a), DefDec (SingleIn b)) => DefDec (SingleIn (a :*: b)) where
  defDec = defDec :*: defDec

instance DefDec (SingleIn a) => DefDec (SingleIn (May a)) where
  defDec = MayP defDec

-- | predicate (==0) for an update result set
type family U0 where
  U0 = UpdN ( 'OEQ ( 'SPos 0))

-- | predicate (==1) for an update result set
type family U1 where
  U1 = UpdN ( 'OEQ ( 'SPos 1))

-- | predicate (>0) for an update result set
type family UGT0 where
  UGT0 = UpdN ( 'OGT ( 'SPos 0))

-- | Allows user to provide a equality predicate at the type level for an update result set
type UEQ :: Nat -> Type
type family UEQ n = w | w -> n where
  UEQ n = UpdN ( 'OEQ ( 'SPos n))

-- | Allows user to provide a "greater then or equal to" predicate at the type level for an update result set
type UGE :: Nat -> Type
type family UGE n = w | w -> n where
  UGE n = UpdN ( 'OGE ( 'SPos n))

-- | Allows user to provide a "greater then" predicate at the type level for an update result set
type UGT :: Nat -> Type
type family UGT n = w | w -> n where
  UGT n = UpdN ( 'OGT ( 'SPos n))

-- | Allows user to provide a "less then or equal to" predicate at the type level for an update result set
type ULE :: Nat -> Type
type family ULE n = w | w -> n where
  ULE n = UpdN ( 'OLE ( 'SPos n))

-- | Allows user to provide a "less then" predicate at the type level for an update result set
type ULT :: Nat -> Type
type family ULT n = w | w -> n where
  ULT n = UpdN ( 'OLT ( 'SPos n))

-- | determines if a database is writeable or not
type WriteableDB :: db -> Bool
type family WriteableDB arg

-- | determines if the resultsets require update access or not
type WriteableRS :: [Type] -> Bool
type family WriteableRS rs where
  WriteableRS '[] = 'True
  WriteableRS (r ': rs) = WriteableOne r BT.&& WriteableRS rs

-- | identifies which resultset type is writeable
type WriteableOne :: Type -> Bool
type family WriteableOne r where
  WriteableOne Upd = 'True
  WriteableOne (UpdN _) = 'True
  WriteableOne (Sel _x) = 'False
  WriteableOne (SelRow _x) = 'False
  WriteableOne (SelCol _x) = 'False
  WriteableOne (SelRowCol _x) = 'False
  WriteableOne SelRaw = 'False
  WriteableOne (Rev a) = WriteableOne a
  WriteableOne (Alle a) = WriteableOne a
  WriteableOne (Some a) = WriteableOne a
  WriteableOne (Exact _n a) = WriteableOne a
  WriteableOne (Range _m _n a) = WriteableOne a
  WriteableOne (May a) = WriteableOne a
  WriteableOne (a :+: b) = WriteableOne a BT.|| WriteableOne b
  WriteableOne (a :*: b) = WriteableOne a BT.&& WriteableOne b
  WriteableOne o = GL.TypeError ( 'GL.Text "WriteableOne: programmer error: unhandled type o=" ':<>: 'GL.ShowType o)

-- | 'ChkLast' fails if you have Alle as the non last element
type ChkLast :: Type -> Bool
type family ChkLast w where
  ChkLast (Alle _a) =
    GL.TypeError
      ( 'GL.Text "Alle has to be the last in the HList"
          ':$$: 'GL.Text "It doesnt make sense to have stuff after it as Alle consumes everything"
      )
  ChkLast (Some _a) =
    GL.TypeError
      ( 'GL.Text "Some has to be the last in the HList"
          ':$$: 'GL.Text "It doesnt make sense to have stuff after it as Some consumes everything"
      )
  ChkLast _a = 'True

-- | convenient group of constraints for validating the dsl
type ValidateNested rs = (MultiLast rs ~ 'True, ValidNestAll rs ~ 'True)

-- | 'MultiLast' checks that if Alle is present then it is the last entry only
type MultiLast :: [Type] -> Bool
type family MultiLast rs where
  MultiLast '[] = 'True
  MultiLast '[_] = 'True
  MultiLast (r ': r1 ': rs) = ChkLast r BT.&& MultiLast (r1 ': rs)

-- | checks that combinations of the dsl are valid for all the resultsets
type ValidNestAll :: [Type] -> Bool
type family ValidNestAll rs where
  ValidNestAll '[] = 'True
  ValidNestAll (r ': rs) = ValidNest r BT.&& ValidNestAll rs

-- | 'ValidNest' checks to see that there are no nested Alle :+: or Range or May ie nested zeroes
type ValidNest :: Type -> Bool
type family ValidNest r where
  ValidNest (Rev a) = ValidNest a
  ValidNest (Alle a) = ValidNest1 a
  ValidNest (Some a) = ValidNest1 a
  ValidNest (Range _m _n a) = ValidNest1 a
  ValidNest (Exact _n a) = ValidNest1 a
  ValidNest (a :+: b) = ValidNest1 a BT.&& ValidNest1 b
  ValidNest (a :*: b) = ValidNest1 a BT.&& ValidNest1 b
  ValidNest (May a) = ValidNest1 a
  ValidNest _a = 'True

-- | checks that combinations of the dsl are valid
type ValidNest1 :: Type -> Bool
type family ValidNest1 r where
  ValidNest1 (Rev a) = ValidNest1 a
  ValidNest1 (Alle a) =
    GL.TypeError
      ( 'GL.Text "Alle is nested within another construct"
          ':$$: 'GL.Text "Doesnt make sense cos either Single or Multiple but not a Single(Multiple) or Multiple(Multiple)"
          ':<>: 'GL.Text " a="
          ':<>: 'GL.ShowType a
      )
  ValidNest1 (Some a) =
    GL.TypeError
      ( 'GL.Text "Some is nested within another construct"
          ':$$: 'GL.Text "Doesnt make sense cos either Single or Multiple but not a Single(Multiple) or Multiple(Multiple)"
          ':<>: 'GL.Text " a="
          ':<>: 'GL.ShowType a
      )
  ValidNest1 (Range _m _n a) = ValidNest a
  ValidNest1 (Exact _n a) = ValidNest a
  ValidNest1 (a :+: b) = ValidNest1 a BT.&& ValidNest1 b
  ValidNest1 (a :*: b) = ValidNest1 a BT.&& ValidNest1 b
  ValidNest1 (May a) =
    GL.TypeError
      ( 'GL.Text "May is nested within another construct"
          ':$$: 'GL.Text "Doesnt make sense cos either Single or Multiple but not a Single(Multiple) or Multiple(Multiple)"
          ':<>: 'GL.Text " a="
          ':<>: 'GL.ShowType a
      )
  ValidNest1 _a = 'True

-- | empty result set message
emptyResultSetMessage :: String
emptyResultSetMessage = "no more resultsets from the server but the type signature expects another resultset"

-- | check that state before and after is valid
checkRss ::
  String ->
  SingleState ->
  SingleState ->
  Either SE (Int, Int, String)
checkRss
  msg
  SingleState{stPos = pos, stRss = rss, stRev = rev}
  SingleState{stPos = posAfter, stRss = rssAfter, stRev = revAfter} = do
    let p =
          if revAfter
            then pos - posAfter
            else posAfter - pos
        r = length rss - length rssAfter -- flipped!
        p2 = show (pos, posAfter)
        r2 = show (length rss, length rssAfter)
        extra = " extra: pos=" ++ p2 ++ " rss=" ++ r2 ++ " rev=" ++ show (rev, revAfter) ++ " r1=" ++ show (posAfter - pos) ++ " r2=" ++ show (pos - posAfter) ++ " p=" ++ show p
    if p < 0
      then failBad ("checkRss:" ++ msg) ("invalid state: p < 0" ++ extra) ""
      else pure (p, r, msg)

-- | checks that there is at least one resultset
expectAtLeastOneResultSet ::
  String ->
  SingleState ->
  SingleState ->
  Either SE ()
expectAtLeastOneResultSet msg' z z' = do
  let msg = "expectAtLeastOneResultSet:" ++ msg'
  (p, _r, extra) <- checkRss msg z z'
  when (p == 0) $ failBad msg ("not consuming any resultsets pos=" ++ show p ++ extra) ""

-- | process the rest of the result sets
handleSomeResultSets ::
  (HasCallStack, Single a) =>
  -- | if nothing then must complete else limit to max number and if failure then exit without an error (Range)
  Maybe Int ->
  String ->
  SingleIn a ->
  Alle a ->
  StateT SingleState (Either SE) (Alle a)
handleSomeResultSets mn msg one sc0 = StateT $ \z -> go (0 :: Int) z sc0
 where
  go !i !z !sc
    | mn == Just i = pure (sc, z)
    | otherwise =
        case stRss z of
          [] -> pure (sc, z)
          _ : _ -> do
            case runStateT (singleCol one) z of
              Left e
                | isJust mn -> pure (sc, z) -- dont die: this is for Range where stuff is optional
                | otherwise -> Left e
              Right (sc', z') -> do
                expectAtLeastOneResultSet (msg ++ ":handleSomeResultSets(" ++ show i ++ ")") z z'
                when (i > 200) $ normalError $ "handleSomeResultSets: looping?? " ++ msg ++ " pos=" ++ show (stPos z')
                go
                  (i + 1)
                  z'
                  (Alle (coerce sc `snocL` sc'))

-- | turn off/on checkRss
_CheckRss :: Bool
_CheckRss = False
