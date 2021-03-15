{-# OPTIONS -Wno-redundant-constraints #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE NoStarIsType #-}
{- |
Module      : HSql.Core.Sql
Description : pure functions for describing, prepocessing and postprocessing Sql
Copyright   : (c) Grant Weyburne, 2016
License     : BSD-3

-}
module HSql.Core.Sql where
import Database.HDBC (SqlValue(..), SqlColDesc(..))
import Database.HDBC.ColTypes (SqlTypeId (SqlUnknownT))
import HSql.Core.Decoder
import HSql.Core.Encoder
import HSql.Core.VinylUtils
import HSql.Core.ErrorHandler
import HSql.Core.Common
import HSql.Core.One
import qualified Data.Vinyl.Functor as V
import qualified Data.Vinyl.Core as V
import qualified Data.Vinyl.Recursive as VR
import Data.Vinyl
import Data.Vinyl.CoRec (CoRec(..))
import Data.Vinyl.TypeLevel hiding (Nat)
import Control.Arrow (left,second)
import Data.Proxy (Proxy(Proxy))
import qualified Data.Text as T
import Data.Text (Text)
import GHC.TypeLits (ErrorMessage((:<>:),(:$$:)),Nat,KnownNat)
import qualified GHC.TypeLits as GL
import Control.Monad (foldM, unless, forM)
import Text.Shakespeare.Text (ToText(toText))
import Data.String (IsString(fromString))
import Data.Text.Internal.Builder (fromText)
import qualified Data.List.NonEmpty as N
import GHC.Generics (Generic)
import Data.Kind (Type)
import qualified PCombinators as P
import PCombinators ((:.:), type (~>), Apply)
import GHC.Stack (HasCallStack)
import Control.DeepSeq (NFData)
import Data.These.Combinators

-- | 'Sql' is the core ADT that holds a vinyl record of encoders for the input
-- and a vinyl record of decoders for the ouput and then the Sql text
data Sql db a b =
  Sql
    { sDescription :: !Text -- ^ description used for logging and error handling
    , sEncoders :: !(Rec Enc a) -- ^ a record of encoders matching the input parameters
    , sDecoders :: !(Rec SingleIn b) -- ^ a record of decoders matching the output columns
    , sSql :: !Text } -- ^ sql
    deriving Generic

instance ToText (Sql db a b) where
  toText = fromText . sSql

instance Show (Sql db a b) where
  show = T.unpack . sSql

hMetaNull :: SqlColDesc
hMetaNull = SqlColDesc "hMetaNull" (SqlUnknownT "dummy type:hMetaNull") (Just 5) (Just 7) (Just 11) (Just True)

-- | 'ST' is a simple state applicative with the order of 'a' and 's' swapped
newtype ST e s a = ST { unST :: s -> Either e (s, a) }

instance Functor (ST e s) where
  fmap f (ST g) =
    ST $ \s -> case g s of
                 Left e -> Left e
                 Right (s', a) -> Right (s', f a)

instance Applicative (ST e s) where
  pure a = ST $ \s -> Right (s, a)
  ST sab <*> ST sa =
    ST $ \s -> case sab s of
       Left e -> Left e
       Right (s', ab) ->
         case sa s' of
           Left e -> Left e
           Right (s'', a) -> Right (s'', ab a)

-- | simple predicates on the return code from a sql update
data Op = OPLT | OPLE | OPEQ | OPGE | OPGT | OPNE deriving (Show, Eq, Generic)

-- | 'UpdN' is similar to 'Upd' but encodes a type level predicate on the return code
newtype UpdN (op :: Op) (val :: Nat) = UpdN Int deriving (Show, Eq, Num, Ord, Generic)
instance NFData (UpdN op val)

-- | 'Upd' holds a return code from a sql update call
newtype Upd = Upd Int deriving (Show, Eq, Num, Ord, Generic)
instance NFData Upd

-- | 'SelOne' holds a single row of data
newtype SelOne a = SelOne { unSelOne :: a } deriving (Show, Eq, Generic)
instance NFData a => NFData (SelOne a)

-- | 'Sel' holds a zero or more rows of data
newtype Sel a = Sel { unSel :: [a] } deriving (Show, Eq, Generic)

-- | 'Alle' is a higher order function that holds zero or more resultsets of the same type
newtype Alle a = Alle { unAlle :: [a] } deriving (Show, Eq, Generic)
instance NFData a => NFData (Alle a)

-- | ':+:' is a higher order function that holds a resultset of type a or of type b
newtype a :+: b = EitherRS { unEitherRS :: Either a b } deriving (Show, Eq, Generic)
instance (NFData a, NFData b) => NFData (a :+: b)

-- | 'Some' is a higher order function that is similar to 'Alle' but holds exactly n resultsets of the same type
--   if rev = 'True then will grab the last n resultsets instead of the first n resultsets
newtype Some (rev :: Bool) (n :: Nat) a = Some { unSome :: [a] } deriving (Show, Eq, Generic)
instance NFData a => NFData (Some rev n a)

type SomeT (n :: Nat) = Some 'False n
type EmosT (n :: Nat) = Some 'True n

type AnyRaw = Upd :+: SelRaw

-- | 'SelRaw' holds a query resultset with undecoded data
newtype SelRaw = SelRaw { unSelRaw :: [[SqlValue]] } deriving (Show, Eq, Generic)
instance NFData SelRaw

-- | 'Single' represents a single result set where 'SingleIn' is the input and 'SingleOut' is the
class Single a where
  data SingleIn a
  type SingleOut a
  showF :: SingleIn a -> String
  singleCol :: SingleIn a
           -> (Int, [ResultSet])
           -> Either SE ((Int, [ResultSet]), ([RMeta],(a, SingleOut a)))

-- | 'Upd' represents any non select query eg dml insert / update / delete o ddl create / drop / alter
instance Single Upd where
  data SingleIn Upd = UpdP
  type SingleOut Upd = Int
  showF UpdP = "UpdP"

  singleCol UpdP (pos,rss) =
    case rss of
      rs1:rss' -> left (CoRec (V.Identity (SingleColE "Upd" (Just pos) "" rss)) N.<|) $ do
        rc <- updImpl rs1
        return ((pos+1,rss'), ([], (Upd rc,rc)))
      [] -> failNR "Upd" pos emptyResultSetMessage

-- | 'UpdN' is similar to 'Upd' but adds a type level predicate on the number of rows returned
instance (ShowOp op, KnownNat val) => Single (UpdN (op :: Op) (val :: Nat)) where
  data SingleIn (UpdN op val) = UpdNP
  type SingleOut (UpdN op val) = Int
  showF _ = let v = P.pnat @val
            in "UpdNP " ++ fst (showOp @op) ++ " " ++ show v

  singleCol UpdNP (pos,rss) =
    case rss of
      rs1:rss' -> left (CoRec (V.Identity (SingleColE "UpdN" (Just pos) "" rss)) N.<|) $ do
        let v = P.pnat @val
        rc <- updImpl rs1
        case second (\x -> x rc v) (showOp @op) of
          (dsp, False) -> failNE "UpdN" pos ("update predicate failed: " ++ show rc ++ " " ++ dsp ++ " " ++ show v)
          (_, True) -> pure ((pos+1,rss'), ([], (UpdN rc,rc)))
      [] -> failNR "UpdN" pos emptyResultSetMessage

-- | 'SelOne' is similar to 'Sel' but expects one row
instance Single (SelOne a) where
  data SingleIn (SelOne a) = SelOneP (Dec a)
  type SingleOut (SelOne a) = a
  showF (SelOneP d) = "SelOneP " ++ show d

  singleCol (SelOneP dec) (pos,rss) =
    case rss of
       rs1:rss' -> left (CoRec (V.Identity (SingleColE "SelOne" (Just pos) "" rss)) N.<|) $ do
         (meta,xxs) <- selImpl rs1 dec
         case xxs of
           [xs] -> return ((pos+1,rss'), ([meta], (SelOne xs,xs)))
           _ -> failSIC "SelOne" (Just pos) ("expected 1 row but found " ++ show (length xxs)) rss
       [] -> failNR "SelOne" pos emptyResultSetMessage

-- | 'Sel' represents a select query
instance Single (Sel a) where
  data SingleIn (Sel a) = SelP (Dec a)
  type SingleOut (Sel a) = [a]
  showF (SelP d) = "SelP " ++ show d

  singleCol (SelP dec) (pos,rss) =
    case rss of
       rs1:rss' -> left (CoRec (V.Identity (SingleColE "Sel" (Just pos) "" rss)) N.<|) $ do
          (meta,a) <- selImpl rs1 dec
          return ((pos+1,rss'), ([meta],(Sel a,a)))
       [] -> failNR "Sel" pos emptyResultSetMessage

-- | 'Alle' expect the rest of the resultsets are of type "a"
instance Single a => Single (Alle a) where
  data SingleIn (Alle a) = AlleP (SingleIn a)
  type SingleOut (Alle a) = [SingleOut a]
  showF (AlleP a) = "AlleP " ++ show a

  singleCol (AlleP one) (pos,rss) =
    let n = length rss
    in left (CoRec (V.Identity (SingleColE "Alle" (Just pos) (show n ++ " resultsets") rss)) N.<|) $ do
         ((ret,wret),rssout) <- foldM (\((as,was),rss') i -> do
                                                 ((i',rss''),(hm,(a,wa))) <- singleCol one (i,rss')
                                                 unless (i' == i+1) $ failBad "Alle Col" ("pos=" ++ show pos ++ " should not happen: not getting a single resultset! " ++ show (i,i')) ""
                                                 return ((as++[(hm,a)],was++[wa]), rss'')
                              ) (([],[]), rss) [1.. n]
         return ((pos+n,rssout), ([], (Alle (map snd ret), wret)))

-- | a sum type on the resultset: ie either type "a" or type "b"
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

-- | represents one or more resultsets of type "a"
instance (Single a, P.GetBool rev, KnownNat n) => Single (Some (rev :: Bool) (n :: Nat) a) where
  data SingleIn (Some rev n a) = SomeP (SingleIn a)
  type SingleOut (Some rev n a) = [SingleOut a]
  showF (SomeP a) = "SomeP" ++ (if P.getBool @rev then " Reverse" else "") ++ "(" ++ show (P.pnat @n) ++  ") " ++ show a

  singleCol w@(SomeP one) (pos,rss) =
     case someImpl w rss of
       Left e -> Left e
       Right (n,msg) ->
         left (CoRec (V.Identity (SingleColE msg (Just pos) "" rss)) N.<|) $ do
           ((ret,wret),rssout) <- foldM (\((as,was),rss') i -> do
                                                   ((i',rss''),(hm,(a,wa))) <- singleCol one (i,rss')
                                                   unless (i' == i+1) $ failBad (msg ++ " Col") ("pos=" ++ show pos ++ " should not happen: not getting a single resultset!" ++ show (i,i')) ""
                                                   return ((as++[(hm,a)],was++[wa]), rss'')
                                ) (([],[]), rss) [1.. n]
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

-- | represents an untyped query resultset
instance Single SelRaw where
  data SingleIn SelRaw = SelRawP
  type SingleOut SelRaw = [[SqlValue]]
  showF SelRawP = "SelRawP"

  singleCol SelRawP (pos,rss) =
    case rss of
      rs1:rss' -> left (CoRec (V.Identity (SingleColE "SelRaw" (Just pos) "" rss)) N.<|) $ do
        (meta,a) <- selRawImpl rs1
        return ((pos+1,rss'), ([meta], (SelRaw a,a)))
      [] -> failNR "Sel" pos emptyResultSetMessage

instance Single a => Show (SingleIn a) where
  show = showF

-- | 'RState' holds the input/ouput for a given resultset
data RState a =
   RState { rsIn :: !(SingleIn a)
          , rsOutWrapped :: a  -- undefined so cannot use strict
          , rsOut :: SingleOut a -- undefined so cannot use strict
          , rsMeta :: ![RMeta]
          }

deriving instance (Show a, Show (SingleIn a), Show (SingleOut a)) => Show (RState a)

-- bearbeiten: how to get around using undefined
-- | 'toRState' sets up the initial state before processing all the resultsets
toRState :: Rec SingleIn rs -> Rec RState rs
toRState = VR.rmap $ \xa -> RState xa
                                   (error "RState: rsOutWrapped doesn't have a value!")
                                   (error "RState: rsOut doesn't have a value!")
                                   []

-- | 'ShowOp' extracts a value level predicate from the typelevel for 'UpdN'
class ShowOp (a :: Op) where
  showOp :: (String, Int -> Int -> Bool)
instance ShowOp 'OPLT where
  showOp = ("<", (<))
instance ShowOp 'OPLE where
  showOp = ("<=", (<=))
instance ShowOp 'OPEQ where
  showOp = ("==", (==))
instance ShowOp 'OPGE where
  showOp = (">=", (>=))
instance ShowOp 'OPGT where
  showOp = (">", (>))
instance ShowOp 'OPNE where
  showOp = ("/=", (/=))

processRetCol
  :: (RecAll RState rs SingleZ
    , ValidateNested rs
    ) => Rec SingleIn rs
      -> [ResultSet]
      -> Either SE (Rec RState rs)
processRetCol decRec = processRetCol' (toRState decRec)

-- | 'processRetCol'' is the same as 'processRet'' but additionally includes metadata
processRetCol'
  :: (RecAll RState rs SingleZ
    , ValidateNested rs
    ) => Rec RState rs
      -> [ResultSet]
      -> Either SE (Rec RState rs)
processRetCol' xs rss =
  case flip unST (0::Int,rss) $ rtraverse (\(V.Compose (V.Dict x)) -> ST (singleColZ x)) (VR.reifyConstraint (Proxy @SingleZ) xs) of
    Left e -> Left e
    Right ((pos,rss'), w) | null rss' -> Right w
                          | otherwise -> failUCC "processRetCol'" pos "Unconsumed" rss'

-- | 'SingleZ' is a simple state monad wrapper around 'Single' which processes a single 'RState' entry
-- we need this wrapper as vinyl uses Rec RState rs wraps RState around each rs
class SingleZ a where
  singleColZ :: a -> (Int, [ResultSet]) -> Either SE ((Int, [ResultSet]), a)

-- only one instance that wraps 'Single' which is needed for 'processRet' and 'processRetCol'
instance Single a => SingleZ (RState a) where
  singleColZ w (b,c) =
    case singleCol (rsIn w) (b,c) of
      Left e -> Left e
      Right (z, (hms,(a,wa))) -> Right (z,w { rsOutWrapped = a, rsOut = wa, rsMeta = hms })

-- | shortcut to get all the hmetas in one go
hmall :: Rec RState '[a] -> [RMeta]
-- cant use Vinyl Const cos no applicative instance!!!!
--hmall rs = L.getConst $ rtraverse (L.Const . rsMeta) rs
hmall = VR.rfoldMap rsMeta

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

-- | convert Rec RState rs to tuple
class PGen (rs :: [Type]) (v :: Type) | rs -> v where
  ext :: Rec RState rs -> SingleOuts rs
instance PGen '[] () where
  ext RNil = ()
instance PGen '[a] a where
  ext (r1 :& RNil) = rsOut r1
instance PGen '[a,b] (a,b) where
  ext (r1 :& r2 :& RNil) = (rsOut r1, rsOut r2)
instance PGen '[a,b,c] (a,b,c) where
  ext (r1 :& r2 :& r3 :& RNil) = (rsOut r1, rsOut r2, rsOut r3)
instance PGen '[a,b,c,d] (a,b,c,d) where
  ext (r1 :& r2 :& r3 :& r4 :& RNil) = (rsOut r1, rsOut r2, rsOut r3, rsOut r4)
instance PGen '[a,b,c,d,e] (a,b,c,d,e) where
  ext (r1 :& r2 :& r3 :& r4 :& r5 :& RNil) = (rsOut r1, rsOut r2, rsOut r3, rsOut r4, rsOut r5)
instance PGen '[a,b,c,d,e,f] (a,b,c,d,e,f) where
  ext (r1 :& r2 :& r3 :& r4 :& r5 :& r6 :& RNil) = (rsOut r1, rsOut r2, rsOut r3, rsOut r4, rsOut r5, rsOut r6)
instance PGen '[a,b,c,d,e,f,g] (a,b,c,d,e,f,g) where
  ext (r1 :& r2 :& r3 :& r4 :& r5 :& r6 :& r7 :& RNil) = (rsOut r1, rsOut r2, rsOut r3, rsOut r4, rsOut r5, rsOut r6, rsOut r7)
instance PGen '[a,b,c,d,e,f,g,h] (a,b,c,d,e,f,g,h) where
  ext (r1 :& r2 :& r3 :& r4 :& r5 :& r6 :& r7 :& r8 :& RNil) = (rsOut r1, rsOut r2, rsOut r3, rsOut r4, rsOut r5, rsOut r6, rsOut r7, rsOut r8)

-- inductive tuples: not as useful as PGen/ext but works for all sizes!
-- () (a,()) (a,(b,())) (a,(b,(c,()))) (a,(b,(c,(d,())))) ...
type family SingleOuts' (rs :: [Type]) :: Type where
  SingleOuts' '[] = ()
  SingleOuts' (a ': as) = (SingleOut a, SingleOuts' as)

-- | alternative approach of converting vinyl record of results to a nested tuple
class PGen' (rs :: [Type]) where
  ext' :: Rec RState rs -> SingleOuts' rs
instance PGen' '[] where
  ext' RNil = ()
instance PGen' as => PGen' (a ': as) where
  ext' (r :& rs) = (rsOut r, ext' rs)

-- | 'selImpl' tries to decode a result set based on the decoder and then runs the predicate
selImpl :: HasCallStack => ResultSet -> Dec a -> Either SE (RMeta, [a])
selImpl z@(Right (meta,xxs)) (Dec dec) = do
  ys <- forM (zip [1::Int ..] xxs) $ \(r,xs) -> do
          case dec xs of
            Left es ->
            -- change the code so only DecodingE happens?
              case justThere $ getDecErrors es of -- ConvE can appear as an exception but seemingly always with DecodingE : see liftCE
                Nothing -> error "selImpl: missing DecodingE" -- cant seem to make this error fire as always has an accompanying DecodingE exception
                Just (ss N.:| _) ->
                  let c = length xs - length (deSqlValues ss)
                  in Left $ liftDE $ decAddError' "selImpl" ("row/col " ++ show (r,c)) [] es
            Right a -> return a
  ret <- forM (zip [1::Int ..] ys) $ \(i,(a,lft)) -> do
    unless (null lft) $ failUCC "Select" i ("selImpl: didnt consume all values! row " ++ show i ++ ":leftovers=" ++ show lft) [z]
    return a
  return (meta, ret)
selImpl z _ = failURST "selImpl(Select)" "expected a select but found an update" z

-- | 'updImpl' processes an Update resultset and runs the predicate
updImpl :: ResultSet -> Either SE Int
updImpl (Left i) = pure i
updImpl z@Right {} = failURST "updImpl(Update)" "expected an update but found a select" z

-- | 'selRawImpl' processes a Query resultset and runs the predicate
selRawImpl :: ResultSet -> Either SE (RMeta, [[SqlValue]])
selRawImpl (Right (meta,xs)) = pure (meta,xs)
selRawImpl z@Left {} = failURST "selRawImpl(Select)" "expected a select but found an update" z

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
selWithDec d = E1 (SelP (decList d))

selOneWithDec :: forall rs
   . DecList rs
   => Rec Dec rs
   -> Rec SingleIn '[SelOne (DecTuples rs)]
selOneWithDec d = E1 (SelOneP (decList d))

-- provide a vinyl list of Decs and then it will convert to the sized tuple
-- of course each defDec must have a type! eg defDec @(Dec String)
-- or better defD @String
-- fd $ runSql pglocal RNil $ selSqlWithDec  (decIp :& defD @String :& RNil) "select '124.11.123.45 xx','abc'"
selSqlWithDec :: forall rs a db
   . (DecList rs, DefEnc (Rec Enc a))
   => Rec Dec rs
   -> Text
   -> Sql db a '[Sel (DecTuples rs)]
selSqlWithDec d = Sql "selSqlWithDec" defEnc (E1 (SelP (decList d)))

selOneSqlWithDec :: forall rs a db
   . (DecList rs, DefEnc (Rec Enc a))
   => Rec Dec rs
   -> Text
   -> Sql db a '[SelOne (DecTuples rs)]
selOneSqlWithDec d = Sql "selOneSqlWithDec" defEnc (E1 (SelOneP (decList d)))

-- you provide a vinyl list of decs and then it will work for a vinyl list result
selSqlWithDecW :: forall rs a db
   . (DecW rs, DefEnc (Rec Enc a))
   => Rec Dec rs
   -> Text
   -> Sql db a '[Sel (WW rs)]
selSqlWithDecW d = Sql "selSqlWithDecW" defEnc (E1 (SelP (decW d)))

selOneSqlWithDecW :: forall rs a db
   . (DecW rs, DefEnc (Rec Enc a))
   => Rec Dec rs
   -> Text
   -> Sql db a '[SelOne (WW rs)]
selOneSqlWithDecW d = Sql "selOneSqlWithDecW" defEnc (E1 (SelOneP (decW d)))

-- for ElFields but you provide a vinyl dec list: still have to have rs defined
-- fd $ runSql pglocal RNil $ selSqlWithDecH @'["aa" ::: IP Int, "bb" ::: String] (decIp :& defD :& RNil) "select '124.11.123.45 xx','abc'"
selSqlWithDecH :: forall rs a db
   . (DecH rs, DefEnc (Rec Enc a))
   => Rec Dec (P.Map P.SndSym0 rs)
   -> Text
   -> Sql db a '[Sel (Rec ElField rs)]
selSqlWithDecH d = Sql "selSqlWithDecH" defEnc (E1 (SelP (decH d)))

-- you provide just the rs with ==: ie labels and all and it figures out the rs result
-- fd $ runSql pglocal RNil $ selSqlWithDecI (#xx ==: decIp :& #yy ==: defD @String :& RNil) "select '124.11.123.45 xx','abc'"
selSqlWithDecI :: forall rs a db
   . (DecI rs, DefEnc (Rec Enc a))
   => Rec ElFieldDec rs
   -> Text
   -> Sql db a '[Sel (Rec ElField rs)]
selSqlWithDecI d = Sql "selSqlWithDecI" defEnc (E1 (SelP (decI d)))

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
  defDec = UpdP
instance DefDec (SingleIn SelRaw) where
  defDec = SelRawP
instance DefDec (Dec a) => DefDec (SingleIn (Sel a)) where
  defDec = SelP defDec
instance DefDec (Dec a) => DefDec (SingleIn (SelOne a)) where
  defDec = SelOneP defDec

instance DefDec (SingleIn a) => DefDec (SingleIn (Alle a)) where
  defDec = AlleP defDec
instance DefDec (SingleIn a) => DefDec (SingleIn (Some rev n a)) where
  defDec = SomeP defDec
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
  WriteableOne (Sel _x) = 'False
  WriteableOne (SelOne _x) = 'False
  WriteableOne SelRaw = 'False
  WriteableOne (Alle a) = WriteableOne a
  WriteableOne (Some _rev _n a) = WriteableOne a
  WriteableOne (a :+: b) = WriteableOne a P.|| WriteableOne b
  WriteableOne o = GL.TypeError ('GL.Text "WriteableOne: programmer error: unhandled type o=" ':<>: 'GL.ShowType o)

data WriteableOneSym0 :: Type ~> Bool
type instance P.Apply WriteableOneSym0 x = WriteableOne x

-- | 'ChkLast' fails if you have Alle as the non last element
type family ChkLast w :: Bool where
  ChkLast (Alle _a) = GL.TypeError ('GL.Text "Alle has to be the last in the HList"
                             ':$$: 'GL.Text "It doesnt make sense to have stuff after it as Alle consumes everything"
                                 )
  ChkLast _a = 'True

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
  ValidNest (Some _rev _n a) = ValidNest1 a
  ValidNest (a :+: b) = ValidNest1 a P.&& ValidNest1 b
  ValidNest _a = 'True

type family ValidNest1 (w :: Type) :: Bool where
  ValidNest1 (Alle _a) = GL.TypeError ('GL.Text "Alle is nested within another construct"
                            ':$$: 'GL.Text "Doesnt make sense cos either Single or Multiple but not a Single(Multiple) or Multiple(Multiple)"
                                 )
  ValidNest1 (Some _rev _n _a) = GL.TypeError ('GL.Text "Some is nested within another construct"
                            ':$$: 'GL.Text "Doesnt make sense cos either Single or Multiple but not a Single(Multiple) or Multiple(Multiple)"
                                 )
  ValidNest1 (a :+: b) = ValidNest1 a P.&& ValidNest1 b
  ValidNest1 _a = 'True

emptyResultSetMessage :: String
emptyResultSetMessage = "no more resultsets from the server but the type signature expects another resultset"
