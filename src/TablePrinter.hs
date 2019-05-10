{-
can wprint 3 different types
Rec ElField rs ie with fields
Rec V.Identity rs  ie without fields
or tuples [One for single values] -- more restrictive than Rec version cos supports One and tuples of size 2..8

'[(Symbol,*)] ie with fields
----------------------------
   Sel (Rec ElField '["c1" ::: Int, "c2" ::: Bool])
== Sel (F '["c1" ::: Int, "c2" ::: Bool]) -- type synonym
== Sel (ToFields' '[Int, Bool])   --- type family to add fields to '[*]  -- max of 8
== Sel (ToFields'' '[Int, Bool])  --- type family to add fields to '[*]  -- no maximum!! but the columnnames are dumb: eg c1 c11 c111 c1111 etc (at least they are distinct but only have GS.AppendSymbol to work with!
== Sel (ToFields (Int, Bool))     --- type family to add fields to One and tuples (upto 8)

'[*] ie without fields
----------------------
   Sel (Rec V.Identity '[Int, Bool])
== Sel (W '[Int,Bool])  -- type synonym

simple One and tuples 2..8  [can do without the One but then doesnt wprint!
--------------------------
Sel (Int,Bool)

the code here works with any Foldable t => t (F rs) which includes Frame (F rs) and [F rs] etc

Frame is Foldable but not traversable as it would need the 'a' to be Traversable to be efficient unless you use toList which is not very efficient
toList ie [F rs] is foldable and since [a] is traversable we can use this with Control.Scanl

eg
a <- loadUsers
wprint $ FS.scan (postscanF @"totx" (FL.premap (V.rvalf #tot) FL.mean)) (toList a)

not sure what implications of overlapping instances are for ZPrint Sel and SelOne
also got rid of qprint: everything runs through wprint but overlapping instances for Sel and SelOne for the fields and no fields version ie new vs old style eg '[Sel (Int,Bool)] vs '[Sel (F '["aa" ::: Int, "bb" ::: Bool])]

wprint
  runSqlRaw         [HRet]
  runSqlRawCol      [HRetCol]
  runSql/runSqlCol  Rec ZZZ rs   -- without fields  eg ZZZ (Sel rs), ZZZ (SelOne rs)  using generic-sop and supporting tuples and One
  Frames [use runSql/runSqlCol with fields then use toFrames to convert to a Frame -- F.Frame (F rs)

-- FIXED with overlapping instance handles all the cases except cant differentiate between Rec ZZZ rs where rs is without fields or with fields
-- this drives wprint
instance Printer [HRet]
instance Printer [HRetCol]
instance Printer (F.Frame (F rs))
instance (V.RecAll ZZZ rs ZPrint) => Printer (Rec ZZZ rs)

-- coltype is only used in toRow : do we need it?

-- singles eg Sel Char doesnt print but Sel (One Char) does!

-- only these tablestyles work: unicodeDdoubleFrameS / unicodeS / asciiS / asciiRoundS
-- the other ones crash on windows cos cant output certain chars
-- need to add a stopper to prttable stuff so we dont try to create a massive string and kill memory:so truncate option on #of rows in a resultset

-- hmap == hliftA hcollapse busts stuff out
-- hcliftA just adds a constraint for all 'a' so hcliftA == hliftA Proxy    thus hclift (Proxy @Show) will work forall a

-- doesnt handle nested tuples eg [(ColDataType, ColumnMeta)] -- we need to cascade down somehow: this is a generics sop thing! look at toRow
 -- tricky: how do we know to go down a level? do we need to call another method that will drop down one level
 -- to distinguish simple tuples from complex ones
-}
-- allow user to take a slice:ie only include certain columns by column number
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS -Wall -Wcompat -Wincomplete-record-updates -Wincomplete-uni-patterns #-}
-- {-# OPTIONS -Wno-redundant-constraints #-}
{- |
Module      : TablePrinter
Description : utilities for displaying resultsets
Copyright   : (c) Grant Weyburne, 2016
License     : GPL-3
Maintainer  : gbwey9@gmail.com

'wprint' is the key function
-}
module TablePrinter where
import qualified Data.ByteString.Char8 as B8
import Data.ByteString (ByteString)
import Data.Data
import qualified Data.Text as T
import Data.Text (Text)
import Text.Layout.Table
import qualified Generics.SOP as GS
import qualified GHC.Generics as G
import Control.Arrow
import Control.Lens hiding (from)
import qualified Control.Lens as L
import Data.Time
import Data.List
import Data.Char
import Data.Semigroup (Sum(..))
import System.IO
import Data.Vinyl
import qualified Data.Vinyl.Recursive as VR
import qualified Data.Vinyl as V
import qualified Data.Vinyl.Functor as V
import qualified Data.Vinyl.TypeLevel as V
import Sql
import qualified Data.Align as A
import Numeric (showHex)
import Database.HDBC.ColTypes
import qualified Safe
import qualified Safe.Exact as SE
import GHC.TypeLits
import GHC.Stack
import qualified Frames as F
import Data.Foldable
import qualified PCombinators as P

-- | a type synonym for functions that change the order of columns displayed or even drop columns
type Fn1 = [(Int, ([String], FType))] -> [Int]

-- | coarse classification of the type of data in a cell
data FType = Stringy | Numy | Datey | Other deriving (Show,Eq,Bounded,Enum,Ord,G.Generic)
-- makePrisms ''FType

-- | how to handle data that is wider than a cell
data FixData = FWrap | FTrunc deriving (Eq,Show,G.Generic)
-- makePrisms ''FixData

-- | how to represent multiple result sets
data Vertical = Vertical | Horizontal deriving (Eq,Show,G.Generic)


-- | type synonym used for functions that transform the text of a cell
type Morph = String -> String

-- | 'Opts' these are the options for displaying resultsets see 'wprintWith'
data Opts = Opts { _oFile       :: !(Maybe FilePath) -- ^ optionally print to a file
                 , _oStyle      :: !TableStyle  -- ^ specify style eg unicode ascii etc
                 , _oFixData    :: !FixData
                 , _oFn1        :: Fn1 -- ^ change the order and number of columns to display
                 , _oVertical :: !Vertical -- ^ align result sets vertically or horizontally
                 , _oRC :: !(Int, Int) -- ^ default row and column size of each cell
                 , _oMorph :: Morph -- ^ transform the text of a cell: used for handling control characters
                 }

makeLenses ''Opts

-- | 'defT' default options
defT :: Opts
defT = Opts { _oFile = Nothing
             , _oStyle = unicodeS
             , _oFixData = FTrunc
             , _oFn1 = defFn1
             , _oVertical = Vertical
             , _oRC = (5,40)
             , _oMorph = defMorph1
             }

defTSimple :: FixData -> Int -> Int -> Morph -> Opts
defTSimple xd r c morph =
  Opts { _oFile = Nothing
       , _oStyle = unicodeS
       , _oFixData = xd
       , _oFn1 = defFn1
       , _oVertical = Vertical
       , _oRC = (r,c)
       , _oMorph = morph
       }

poStyle :: TableStyle -> Opts -> Opts
poStyle ts o = o { _oStyle = ts }

poMorph :: Morph -> Opts -> Opts
poMorph m o = o { _oMorph = m }

-- | 'poRC' allows you to adjust the max number of subrows and columns within a cell
poRC :: ((Int, Int) -> (Int, Int)) -> Opts -> Opts
poRC f o = o & oRC %~ f

-- | 'poR' allows you to adjust the max number of subrows within a cell
poR :: (Int -> Int) -> Opts -> Opts
poR f o = o & oRC . _1 %~ f

-- | 'poC' allows you to adjust the max columns size with a cell
poC :: (Int -> Int) -> Opts -> Opts
poC f o = o & oRC . _2 %~ f

-- | 'poCols' allows you to change which columns get shown
poCols :: ([Int] -> [Int]) -> Opts -> Opts
poCols fn o = o { _oFn1 = fn . map fst }

-- | 'poFile' allows you send the output to a file
poFile :: FilePath -> Opts -> Opts
poFile fn o = o { _oFile = Just fn }

-- | 'poHorizontal' allows you to jam multiple results horizontally
poHorizontal :: Opts -> Opts
poHorizontal o = o { _oVertical = Horizontal }

-- | 'poTrunc' truncates the data in each cell if it is too large
poTrunc :: Opts -> Opts
poTrunc o = o { _oFixData = FTrunc }

-- | 'poWrap' wraps the data in each cell if it is too large
poWrap :: Opts -> Opts
poWrap o = o { _oFixData = FWrap }

-- | 'poAscii' uses ascii instead of unicode (on windows is useful when writing to a file)
poAscii :: Opts -> Opts
poAscii o = o { _oStyle = asciiS }

hexChar :: Char -> String
hexChar c =
  let (a,b) = quotRem (ord c) 16
  in showHex a (showHex b "")

-- | simple utility for chunking data
chunksOf :: Int -> [a] -> [[a]]
chunksOf n = unfoldr f
  where f xs | null xs = Nothing
             | otherwise = Just (splitAt n xs)

-- | horizontal: tries to join resultsets horizontally
prttableRecH :: (V.RecAll ZZZ rs ZPrint) => Opts -> Rec ZZZ rs -> String
prttableRecH o xs =
  let ret = VR.reifyConstraint (Proxy @ZPrint) xs
      (cs,hs,MMM rs) = getConst $ VR.rfoldMap (\(V.Compose (V.Dict x)) -> Const $ zprintH x o) ret
  in tableString cs (_oStyle o) (titlesH hs) (map (colsAllG top) rs)

-- | prints resultsets as they appear (vertically)
prttableRecV :: (V.RecAll ZZZ rs ZPrint) => Opts -> Rec ZZZ rs -> String
prttableRecV o xs =
  let ret = VR.reifyConstraint (Proxy @ZPrint) xs
--  in concat $ V.recordToList $ snd $ flip unSTI (0::Int) $ V.rtraverse (\(V.Compose (V.Dict x)) -> STI $ \pos -> fmap V.Const (zprintV x rsHeader1 pos)) ret
  in VR.rfoldMap V.getConst $ snd $ flip unSTI (0::Int) $ V.rtraverse (\(V.Compose (V.Dict x)) -> STI $ \pos -> fmap V.Const (zprintV x o rsHeader1 pos)) ret

-- | 'STI' simple state applicative
newtype STI s a = STI { unSTI :: s -> (s, a) }
instance Functor (STI s) where
  fmap f (STI g) = STI $ \s -> let (s', a) = g s in (s', f a)
instance Applicative (STI s) where
  pure a = STI (,a)
  STI sab <*> STI sa = STI $ \s -> let (s', ab) = sab s
                                       (s'', a) = sa s'
                                   in (s'', ab a)

-- | describes the information needed for the resultset header
type FnRSHeader = (String,Int) -> String

prefixMessage :: FnRSHeader -> String -> FnRSHeader
prefixMessage k s (ss,ii) = k (s <> ss,ii)

rsHeader1 :: FnRSHeader
rsHeader1 (s,i) = "\n\nresultset " <> show (i+1) <> " " <> s <> "\n"

rsHeader2 :: FnRSHeader
rsHeader2 = const "\n"

class ZPrint a where
  -- | displays the results horizontally
  zprintH :: HasCallStack => a -> Opts -> ([ColSpec], [String], MMM [String])
  -- | displays the results vertically (normal usecase)
  zprintV :: HasCallStack => a -> Opts -> FnRSHeader -> Int -> (Int, String)

instance (KnownNat (P.Length rs), ReifyConstraint FromField V.Identity rs, RFoldMap rs)
     => ZPrint (ZZZ (SelOne (Rec V.Identity rs))) where
  zprintH (ZZZ _ _ a meta) o =
     qprttableH o (addColumnNameUsingMeta (recLen @rs) (concat meta)) [a]
  zprintV (ZZZ _ _ a meta) o fn pos = -- can use 'a' instead of Proxy @(F rs) but for some reason doesnt work for SelOne?
     (pos+1, fn ("Sel",pos) <> qprttableV o (addColumnNameUsingMeta (recLen @rs) (concat meta)) [a])

instance (F.ColumnHeaders rs, ReifyConstraint FromField V.Identity (Unlabeled rs), StripFieldNames rs, RFoldMap (Unlabeled rs))
     => ZPrint (ZZZ (SelOne (F rs))) where
  zprintH (ZZZ _ _ a meta) o =
     qprttableH o (addMetaToColName (F.columnHeaders (Proxy @(F rs))) (concat meta)) [stripNames a]
  zprintV (ZZZ _ _ a meta) o fn pos =
     (pos+1, fn ("SelOne",pos) <> qprttableV o (addMetaToColName (F.columnHeaders (Proxy @(F rs))) (concat meta)) [stripNames a])

instance {-# OVERLAPPABLE #-} (GS.HasDatatypeInfo a, GS.Generic a, GS.Code a ~ '[xs], GS.All FromField xs)
     => ZPrint (ZZZ (SelOne a)) where
  zprintH (ZZZ _ _ a meta) o = prttableH o meta [a]
  zprintV (ZZZ _ _ a meta) o fn pos = (pos+1, fn ("SelOne",pos) <> prttableV o meta [a])

instance (KnownNat (P.Length rs), ReifyConstraint FromField V.Identity rs, RFoldMap rs)
     => ZPrint (ZZZ (Sel (Rec V.Identity rs))) where
  zprintH (ZZZ _ _ a meta) o =
     qprttableH o (addColumnNameUsingMeta (recLen @rs) (concat meta)) a
  zprintV (ZZZ _ _ a meta) o fn pos = -- can use 'a' instead of Proxy @(F rs) but for some reason doesnt work for SelOne?
     (pos+1, fn ("Sel",pos) <> qprttableV o (addColumnNameUsingMeta (recLen @rs) (concat meta)) a)

instance (F.ColumnHeaders rs, ReifyConstraint FromField V.Identity (Unlabeled rs), StripFieldNames rs, RFoldMap (Unlabeled rs))
     => ZPrint (ZZZ (Sel (F rs))) where
  zprintH (ZZZ _ _ a meta) o =
     qprttableH o (addMetaToColName (F.columnHeaders a) (concat meta)) (map stripNames a)
  zprintV (ZZZ _ _ a meta) o fn pos = -- can use 'a' instead of Proxy @(F rs) but for some reason doesnt work for SelOne?
     (pos+1, fn ("Sel",pos) <> qprttableV o (addMetaToColName (F.columnHeaders a) (concat meta)) (map stripNames a))

instance {-# OVERLAPPABLE #-}(GS.HasDatatypeInfo a, GS.Generic a, GS.Code a ~ '[xs], GS.All FromField xs)
    => ZPrint (ZZZ (Sel a)) where
  zprintH (ZZZ _ _ a meta) o = prttableH o meta a
  zprintV (ZZZ _ _ a meta) o fn pos = (pos+1, fn ("Sel",pos) <> prttableV o meta a)

instance ZPrint (ZZZ SelRaw) where
  zprintH (ZZZ _ _ a meta) o = prttableHSelRaw o meta a -- globs stuff into one field but not used much anyway!
  zprintV (ZZZ _ _ a meta) o fn pos = (pos+1, fn ("SelRaw",pos) <> concat (if null meta then prtHRet o [Right @Int a] else prtHRetCol o [Right @Int (head meta, a)])) -- delegate to wprint ie prtHRet

instance ZPrint (ZZZ Upd) where
  zprintH (ZZZ _ _ rc _) _ = ([upto 20], ["Upd"], MMM [[["Upd rc=" ++ show rc]]])
  zprintV (ZZZ _ _ rc _) _ fn pos = (pos+1, fn ("Upd",pos) <> "Upd " <> show rc)

instance (ShowOp op, KnownNat val) => ZPrint (ZZZ (UpdN op val)) where
  zprintH (ZZZ _ _ rc _) _ = ([upto 20], [show (getPred @op (P.pnat @val))], MMM [[["UpdN rc=" ++ show rc]]])
  zprintV (ZZZ _ _ rc _) _ fn pos = (pos+1, fn (show (getPred @op (P.pnat @val)), pos) <> "UpdN " <> show rc)

instance (ZPrint (ZZZ a), ZPrint (ZZZ b)) => ZPrint (ZZZ (a :+: b)) where
  zprintH (ZZZ (x :+: _) (EitherRS (Left w)) (Left a) _) o = zprintH (ZZZ x w a []) o
  zprintH (ZZZ (_ :+: y) (EitherRS (Right w)) (Right a) _) o = zprintH (ZZZ y w a []) o
  zprintH (ZZZ (_ :+: _) (EitherRS _) _ _) _ = error "Left and Right got mixed somehow in zprintH x :+: y"

  zprintV (ZZZ (x :+: _) (EitherRS (Left w)) (Left a) _) o fn pos = zprintV (ZZZ x w a []) o (prefixMessage fn "Left ") pos
  zprintV (ZZZ (_ :+: y) (EitherRS (Right w)) (Right a) _) o fn pos = zprintV (ZZZ y w a []) o (prefixMessage fn "Right ") pos
  zprintV (ZZZ (_ :+: _) (EitherRS _) _ _) _ _ _ = error "Left and Right got mixed somehow in zprintV x :+: y"

-- need to prefix Alle info for each zprint
instance ZPrint (ZZZ a) => ZPrint (ZZZ (Alle a)) where
  zprintH (ZZZ (AlleP x _) (Alle xs) ys _ ) o = mconcat $ zipWith (\a b -> zprintH (ZZZ x a b []) o) xs ys
  zprintV (ZZZ (AlleP x _) (Alle xs) ys _ ) o fn pos = (pos+length xs,) $ mconcat $ map snd $ zipWith3 (\a b i -> zprintV (ZZZ x a b []) o (prefixMessage fn "Alle ") i) xs ys [pos..]

instance (KnownNat n, ZPrint (ZZZ a)) => ZPrint (ZZZ (Some n a)) where
  zprintH (ZZZ (SomeP x _) (Some xs) ys _ ) o = mconcat $ zipWith (\a b -> zprintH (ZZZ x a b []) o) xs ys
  zprintV (ZZZ (SomeP x _) (Some xs) ys _ ) o fn pos = (pos+length xs,) $ mconcat $ map snd $ zipWith3 (\a b i -> zprintV (ZZZ x a b []) o (prefixMessage fn ("Some " <> show (P.pnat @n) <> " ")) i) xs ys [pos..]

getFieldNames :: GS.HasDatatypeInfo a => proxy a -> [String]
getFieldNames p =
  case GS.datatypeInfo p of
    GS.ADT     _ _ cs -> fNms cs
    GS.Newtype _ _ c -> fNms $ c GS.:* GS.Nil

fNms :: HasCallStack => GS.NP GS.ConstructorInfo a -> [String]
fNms (GS.Record _ xs GS.:* _) = fNmsRec xs
fNms ((GS.Constructor _ :: GS.ConstructorInfo z) GS.:* _) = let ll = GS.lengthSList (Proxy @z) in map (("?"++).show) [0..ll-1]
fNms GS.Nil = error "impossible case: fNms Nil"
fNms (GS.Infix {} GS.:* _) = error "impossible case: fNms Infix"

fNmsRec :: GS.NP GS.FieldInfo a -> [String]
fNmsRec GS.Nil = []
fNmsRec (GS.FieldInfo nm GS.:* rest) = nm : fNmsRec rest

convstring :: FromField a => Opts -> a -> [[String]]
convstring o a =
  let xxs = fromField a
  in map (flattenCell o . lines . _oMorph o) xxs

-- | Defines the supported types that can be displayed using wprint
class FromField a where
  fromField :: a -> [String]
  coltype :: Int -> a -> [ColSpec]
  coltype _ _ = [def]
  fieldtype :: p a -> a -> [FType]
  fieldtype _ = const [Other]

-- no dependencies on elements having a FromField instance so we display anything in a tuple that is showable
instance (Show a1, Show a2)
  => FromField (a1, a2) where
  fromField z = [show z]
  coltype i z = coltype i (show z)
  fieldtype _ _ = fieldtype (Proxy @String) ""

instance (Show a1, Show a2, Show a3)
  => FromField (a1, a2, a3) where
  fromField z = [show z]
  coltype i z = coltype i (show z)
  fieldtype _ _ = fieldtype (Proxy @String) ""

instance (Show a1, Show a2, Show a3, Show a4)
  => FromField (a1, a2, a3, a4) where
  fromField z = [show z]
  coltype i z = coltype i (show z)
  fieldtype _ _ = fieldtype (Proxy @String) ""

instance (Show a1, Show a2, Show a3, Show a4, Show a5)
  => FromField (a1, a2, a3, a4, a5) where
  fromField z = [show z]
  coltype i z = coltype i (show z)
  fieldtype _ _ = fieldtype (Proxy @String) ""

instance (Show a1, Show a2, Show a3, Show a4, Show a5, Show a6)
  => FromField (a1, a2, a3, a4, a5, a6) where
  fromField z = [show z]
  coltype i z = coltype i (show z)
  fieldtype _ _ = fieldtype (Proxy @String) ""

instance FromField t => FromField (ElField '(s, t)) where
  fromField (Field v) = fromField @t v
  coltype i (Field v) = coltype i v
  fieldtype _ (Field v) = fieldtype Proxy v

instance FromField a => FromField (V.Identity a) where
  fromField = fromField . V.getIdentity
  coltype i = coltype i . V.getIdentity
  fieldtype _ (V.Identity a) = fieldtype Proxy a

instance FromField a => FromField (L.Identity a) where
  fromField = fromField . L.runIdentity
  coltype i = coltype i . L.runIdentity
  fieldtype _ (L.Identity a) = fieldtype Proxy a

instance FromField [SqlValue] where
  fromField = concatMap fromField
  coltype i = concatMap (coltype i)
  fieldtype _ = concatMap (fieldtype Proxy)

instance FromField SqlValue where
  fromField = \case
                 SqlInt32 i -> fromField (fromIntegral @_ @Integer i)
                 SqlBool b -> fromField b
                 SqlInteger i -> fromField i
                 SqlString s -> fromField s
                 SqlByteString bs -> fromField bs
                 SqlInt64 i -> fromField (fromIntegral @_ @Integer i)
                 SqlWord32 i -> fromField (fromIntegral @_ @Integer i)
                 SqlWord64 i -> fromField (fromIntegral @_ @Integer i)
                 SqlChar c -> fromField c
                 SqlDouble i -> fromField i
                 SqlRational r -> fromField r
                 SqlLocalDate day -> fromField day
                 SqlLocalTimeOfDay i -> fromField i
                 SqlZonedLocalTimeOfDay i j -> ["SqlZonedLocalTimeOfDay:" <> show (i,j)]
                 SqlLocalTime loc -> fromField loc
                 SqlZonedTime zt -> fromField zt
                 SqlUTCTime u -> fromField u
                 SqlDiffTime nom -> fromField nom
                 SqlPOSIXTime nom -> fromField nom
                 SqlEpochTime i -> ["SqlEpochTime:" <> show i]
                 SqlTimeDiff i -> ["SqlTimeDiff:" <> show i]
                 SqlNull -> ["<null>"]

  coltype j = \case
                 SqlInt32 i -> coltype j (fromIntegral @_ @Integer i)
                 SqlBool b -> coltype j b
                 SqlInteger i -> coltype j i
                 SqlString s -> coltype j s
                 SqlByteString bs -> coltype j bs
                 SqlInt64 i -> coltype j (fromIntegral @_ @Integer i)
                 SqlWord32 i -> coltype j (fromIntegral @_ @Integer i)
                 SqlWord64 i -> coltype j (fromIntegral @_ @Integer i)
                 SqlChar c -> coltype j c
                 SqlDouble i -> coltype j i
                 SqlRational r -> coltype j r
                 SqlLocalDate day -> coltype j day
                 SqlLocalTimeOfDay i -> coltype j i
                 SqlZonedLocalTimeOfDay z _ -> coltype j z
                 SqlLocalTime loc -> coltype j loc
                 SqlZonedTime zt -> coltype j zt
                 SqlUTCTime u -> coltype j u
                 SqlDiffTime nom -> coltype j nom
                 SqlPOSIXTime nom -> coltype j nom
                 SqlEpochTime i -> coltype j i
                 SqlTimeDiff i -> coltype j i
                 SqlNull -> coltype j ("<null>" :: String)

  fieldtype _ = \case
                 SqlInt32 _ -> [Numy]
                 SqlBool _ -> [Other]
                 SqlInteger _ -> [Numy]
                 SqlString _ -> [Stringy]
                 SqlByteString _ -> [Stringy]
                 SqlInt64 _ -> [Numy]
                 SqlWord32 _ -> [Numy]
                 SqlWord64 _ -> [Numy]
                 SqlChar _ -> [Stringy]
                 SqlDouble _ -> [Numy]
                 SqlRational _ -> [Numy]
                 SqlLocalDate _ -> [Datey]
                 SqlLocalTimeOfDay _ -> [Datey]
                 SqlZonedLocalTimeOfDay _ _ -> [Datey]
                 SqlLocalTime _ -> [Datey]
                 SqlZonedTime _ -> [Datey]
                 SqlUTCTime _ -> [Datey]
                 SqlDiffTime _ -> [Other]
                 SqlPOSIXTime _ -> [Datey]
                 SqlEpochTime _ -> [Datey]
                 SqlTimeDiff _ -> [Other]
                 SqlNull -> [Other]

instance FromField NominalDiffTime where
  fromField = (:[]) . show
instance FromField TimeOfDay where
  fromField = (:[]) . show
instance FromField Rational where
  fromField = (:[]) . show
  coltype _ = const [numCol]
  fieldtype _ = const [Numy]
instance FromField Bool where
  fromField = (:[]) . show
instance FromField Char where
  fromField = (:[]) . (:[])
instance FromField ByteString where
  fromField = (:[]) . B8.unpack
  coltype i = const [upto i]
  fieldtype _ = const [Stringy]
instance FromField String where
  fromField = (:[])
  coltype i = const [upto i]
  fieldtype _ = const [Stringy]
instance FromField Text where
  fromField = (:[]) . T.unpack
  coltype i = const [upto i]
  fieldtype _ = const [Stringy]
instance FromField Int where
  fromField = (:[]) . show
  coltype _ = const [numCol]
  fieldtype _ = const [Numy]
instance FromField Integer where
  fromField = (:[]) . show
  coltype _ = const [numCol]
  fieldtype _ = const [Numy]
instance FromField Float where
  fromField = (:[]) . show
  coltype _ = const [numCol]
  fieldtype _ = const [Numy]
instance FromField Double where
  fromField = (:[]) . show
  coltype _ = const [numCol]
  fieldtype _ = const [Numy]
instance FromField UTCTime where
  fromField = (:[]) . formatTime defaultTimeLocale "%F %T"
  fieldtype _ = const [Datey]
instance FromField ZonedTime where
  fromField = (:[]) . formatTime defaultTimeLocale "%F %T"
  fieldtype _ = const [Datey]
instance FromField Day where
  fromField = (:[]) . formatTime defaultTimeLocale "%F"
  fieldtype _ = const [Datey]
instance FromField LocalTime where
  fromField = (:[]) . formatTime defaultTimeLocale "%F %T"
  fieldtype _ = const [Datey]
instance FromField a => FromField (Maybe a) where
  fromField = maybe ["<null>"] fromField
  coltype i = maybe [def] (coltype i) -- can we use numCol even if "null"
  fieldtype _ = const (fieldtype (Proxy @a) undefined)

upto :: Int -> ColSpec
upto i = column (expandUntil i) Text.Layout.Table.left def def

toRow :: (HasCallStack, GS.Generic a, GS.Code a ~ '[xs], GS.All FromField xs)
   => Opts -> a -> [(ColSpec, [String], FType)]
toRow o a =
  case GS.from a of
    GS.SOP (GS.Z xs) -> let ret = GS.hcollapse (GS.hcliftA ff (\(GS.I v) -> GS.K (coltype (_oRC o ^. _2) v, convstring o v, fieldtype (v <$ Proxy) v)) xs)
                  in concatMap (\(as,bs,cs) -> SE.zip3Exact as bs cs) ret
    GS.SOP (GS.S _) -> error "impossible case: toRow SOP S"
  where ff = Proxy :: Proxy FromField

toColSqlValue :: Opts -> SqlValue -> (ColSpec, [String], FType)
toColSqlValue o x =
   (head $ coltype (_oRC o ^. _2) x, head $ convstring o x, head $ fieldtype Proxy x)

defMorph1, defMorph2, defMorph3, defMorph4, defMorph5, defMorph6 :: Morph
defMorph1 = morph1 ("\n", "", " ") (morphFn1 False)
defMorph2 = morph1 ("\n", "\\r", "\\t") (morphFn1 False)
defMorph3 = morph1 (" ", "\\r", "\\t") (morphFn1 False)
defMorph4 = morph1 ("\\n", "\\r", "\\t") (morphFn1 False)
defMorph5 = morph1 ("\\n", "\\r", "\\t") (morphFn1 True)
defMorph6 = morph1 ("\n", "\\r", "\\t") (morphFn1 True)

morphFn1 :: Bool -> Char -> String
morphFn1 hideControlChars c = if hideControlChars then [] else "?" <> hexChar c <> "?"

morph1 :: (String, String, String) -> (Char -> String) -> Morph
morph1 (controlN, controlR, controlT) fn =
  concatMap (\case
               '\n' -> controlN
               '\r' -> controlR
               '\t' -> controlT
               c | isControl c -> fn c
               o -> [o])

-- todo: somehow need to use upto to show truncation [upto 20] set it to c-1 and make sure not 0
-- r c have to be reasonable values ie r=1 and c=2 minimum
-- show row truncation somehow!
flattenCell :: Opts -> [String] -> [String]
flattenCell o s1 =
  let (r,c) = _oRC o
      s2 = case _oFixData o of
              FWrap -> concatMap (chunksOf c) s1
              FTrunc -> map (take (c+1)) s1 -- set upto to value of c
      s3 = take r s2
  in if length s2 > r then s3 & _Snoc . _2 %~ (<> "~") else s3
--  in s3 & if length s2 > r then over (_Snoc . _2) (<> "~") else id
{-
>["abc"::String,"defg"] & _Snoc . _2 . _Snoc . _2 .~ 'z'
["abc","defz"]
-}

-- use \r \n \t for xlations
-- premorph -- eg remove newlines / change control chars except newlines / do nothing (default and most common)
-- max rows / max cols / truncation marks / escape control
-- need to indicate that we truncated a col and a line! ie ^ for a col and ~ for lines if both then just use ~
-- has to be at the end of the previous line

prttableHSelRaw :: HasCallStack
  => Opts -> [HMeta] -> [[SqlValue]] -> ([ColSpec], [String], MMM [String])
prttableHSelRaw _ _ [] = ([upto 20], ["oops"], MMM [[["no data"]]])
prttableHSelRaw o meta ts =
  case (fmap.fmap) (toColSqlValue o) ts of
    zs@(z' : _) ->
      let is = squarble (_oFn1 o) ((map.map) (view _2 &&& view _3) zs)
          cols = map (view _1 . Safe.atNote "prttableHSelRaw cols" z') is
          flds = if null meta then zipWith (\x v -> v <> "_" <> show x) [1::Int ..] (getColumnTypes ts)
          -- zipWith (\i x -> head (words (show x)) ++ "_" ++ show i) [1::Int ..] (head ts)
                 else map fst $ head meta
      in (cols
         ,map (Safe.atNote ("prttableHSelRaw flds=" ++ show flds ++ " is=" ++ show is) flds) is
         ,MMM (map (\z -> map (view _2 . Safe.atNote "prttableHSelRaw MMM" z) is) zs))
    _ -> error "prttableHSelRaw empty list!"

prttableH :: forall xs a . (GS.HasDatatypeInfo a, GS.Generic a, GS.Code a ~ '[xs], GS.All FromField xs, HasCallStack)
  => Opts -> [HMeta] -> [a] -> ([ColSpec], [String], MMM [String])
prttableH _ _ [] = ([upto 20], ["oops"], MMM [[["no data"]]])
prttableH o meta ts =
  case map (toRow o) ts of
    zs@(z' : _) ->
      let is = squarble (_oFn1 o) ((map.map) (view _2 &&& view _3) zs)
          cols = map (view _1 . Safe.atNote "prttableH cols" z') is
          flds = if null meta then getFieldNames (Proxy @a) else map fst $ head meta
      in (cols
         ,map (Safe.atNote ("prttableH flds=" ++ show flds ++ " is=" ++ show is) flds) is
         ,MMM (map (\z -> map (view _2 . Safe.atNote "prttableH MMM" z) is) zs))
    _ -> error "prttableH empty list!"

prttableV :: forall xs a . (GS.HasDatatypeInfo a, GS.Generic a, GS.Code a ~ '[xs], GS.All FromField xs, HasCallStack)
  => Opts -> [HMeta] -> [a] -> String
prttableV _ _ [] = "*** no data ***"
prttableV o meta ts =
  case map (toRow o) ts of
    zs@(z' : _) ->
      let is = squarble (_oFn1 o) ((map.map) (view _2 &&& view _3) zs)
          cols = map (view _1 . Safe.atNote "prttableV cols" z') is
          flds' = if null meta then getFieldNames (Proxy @a) else map fst $ head meta
          flds = if length is > length flds' then take (length is) (zipWith (\i n -> n <> show i) [1::Int ..] (cycle flds'))
                 else flds'
      in tableString cols
         (_oStyle o)
         (titlesH $ map (Safe.atNote ("prttableV flds " ++ show flds ++ " is=" ++ show is) flds) is)
         (map (\z -> colsAllG top (map (view _2 . Safe.atNote "prttableV colsAllG" z) is)) zs)
    _ -> error "prttableV empty list!"

whatcoltype :: Int -> SqlTypeId -> ColSpec
whatcoltype i x | x `elem` [SqlCharT, SqlVarCharT, SqlLongVarCharT, SqlWCharT, SqlWVarCharT, SqlWLongVarCharT]
                    = upto i
                | x `elem` [SqlDecimalT, SqlNumericT, SqlSmallIntT, SqlIntegerT, SqlRealT, SqlFloatT, SqlDoubleT, SqlBitT, SqlTinyIntT, SqlBigIntT]
                    = numCol
                | x `elem` [SqlBinaryT, SqlVarBinaryT, SqlLongVarBinaryT]
                   = upto i
                | x `elem` [SqlDateT, SqlTimeT, SqlTimeWithZoneT, SqlTimestampT, SqlTimestampWithZoneT, SqlUTCDateTimeT, SqlUTCTimeT]
                   = def
                | x == SqlGUIDT
                   = def
                | otherwise = def

wprint' :: Printer a => a -> IO ()
wprint' = wprintWith (poAscii defT)

wprintWith' :: Printer a => Opts -> a -> IO ()
wprintWith' o = wprintWith (poAscii o)

-- | 'wprint' displays sql output to the screen using default settings 'defT'
wprint :: Printer a => a -> IO ()
wprint = wprintWith defT

wprintfn :: Printer a => FilePath -> a -> IO ()
wprintfn fn = wprintWith (poFile fn defT)

wprintfn' :: Printer a => FilePath -> a -> IO ()
wprintfn' fn = wprintWith (poFile fn (poAscii defT))

wprintCols :: Printer a => ([Int] -> [Int]) -> a -> IO ()
wprintCols fn = wprintWith (poCols fn defT)

wprintCols' :: Printer a => ([Int] -> [Int]) -> a -> IO ()
wprintCols' fn = wprintWith (poCols fn (poAscii defT))

wprintWith :: Printer a => Opts -> a -> IO ()
wprintWith o xs = case _oFile o of
                    Nothing -> putStrLn $ unlines $ wfn o xs
                    Just fn -> withFile fn AppendMode $ \h -> do
                                 hSetEncoding h utf8
                                 hPutStrLn h $ unlines $ wfn o xs

-- | 'Printer' directs the to the implementation that handles printing
class Printer a where
  wfn :: Opts -> a -> [String]
instance Printer [HRet] where
  wfn = prtHRet
instance Printer [HRetCol] where
  wfn = prtHRetCol
instance (Foldable t, ReifyConstraint FromField V.Identity rs, RFoldMap rs)
   => Printer (t (Rec V.Identity rs)) where
  wfn o a = [fprttableRec' o a]
-- bearbeiten: do we need this
instance (Foldable t, RFoldMap rs, ReifyConstraint FromField V.Identity rs, RMap rs)
   => Printer (t (Rec L.Identity rs)) where
  wfn o a = [fprttableRec' o (map (V.rmap (V.Identity . L.runIdentity)) (toList a))]
instance (Foldable t, StripFieldNames rs, F.ColumnHeaders rs, ReifyConstraint FromField V.Identity (Unlabeled rs), RFoldMap (Unlabeled rs))
   => Printer (t (F rs)) where
  wfn o a = [fprttableRecT o a]
instance (V.RecAll ZZZ rs ZPrint) => Printer (Rec ZZZ rs) where
  wfn o a = [(if _oVertical o == Vertical then prttableRecV else prttableRecH) o a]

-- | 'prtHRetCol' handles printing untyped resultsets with metadata
prtHRetCol :: Opts -> [HRetCol] -> [String]
prtHRetCol _ [] = ["nothing to print"]
prtHRetCol o lrs =
  let prefix i = "\n" <> show i <> " of " <> show (length lrs) <> " "
  in flip map (zip [1::Int ..] lrs) $ \case
          (i,Left rc) -> prefix i <> "Update rc=" <> show rc
          (i,Right (cs,rss)) ->
--              let cols = map (whatcoltype (_oLens o) . colType . snd) cs
              let cols = map (const (upto (_oRC o ^. _2))) cs
                  rows = map (concatMap (convstring o)) rss
--                  rows = flip map rss $ \rs ->
--                            map (map concat . convstring o) rs
              in prefix i <> "Select " <> show (length rows) <> " rows\n"
                          <> tableString cols
                             (_oStyle o)
                             (titlesH $ map fst cs)
                             (map (colsAllG top) rows)

-- | 'prtHRet' handles printing untyped resultsets without metadata
prtHRet :: Opts -> [HRet] -> [String]
prtHRet _ [] = ["nothing to print"]
prtHRet o lrs =
  let prefix i = show i <> " of " <> show (length lrs) <> " "
  in flip map (zip [1::Int ..] lrs) $ \case
          (i,Left rc) -> prefix i <> "Update rc=" <> show rc
          (i,Right []) -> prefix i <> "Select *** no rows ***"
          (i,Right rss@(rs:_)) ->
          -- todo: probably not worth using coltype to get ColSpec:just use info from convstring that tells us the width
              let cols = map (const (upto (_oRC o ^. _2))) rs
                  rows = map (concatMap (convstring o)) rss
              in prefix i <> "Select " <> show (length rows) <> " rows\n"
                          <> tableString cols
                             (_oStyle o)
                             (titlesH $ zipWith (\x v -> v <> "_" <> show x) [1::Int ..] (getColumnTypes rss))
                             (map (colsAllG top) rows)

getColumnTypes :: [[SqlValue]] -> [String]
getColumnTypes rrs =
  let nn = show SqlNull
  in flip map (transpose rrs) $ \cs ->
      let ss = map (head . words . show) cs
      in drop 3 $ Safe.headDef nn (dropWhile (==nn) ss)

defFn1 :: Fn1
defFn1 = map fst

-- useful with eg (take 4) for first 4 cols only or (drop 2)
colFn1 :: ([Int] -> [Int]) -> Fn1
colFn1 fn = fn . map fst

-- excludes any column where the length of the sum of all the strings >=i
lengthFn1 :: Int -> Fn1
lengthFn1 i =  map (view _1) . filter ((<i) . length . concat . view (_2 . _1))

-- | exclude columns if any field is over a certain size
lengthAnyFn1 :: Int -> Fn1
lengthAnyFn1 i =  map (view _1) . filter ((<i) . Safe.maximumDef 0 . map length . view (_2 . _1))

-- | 'typeFn1' allows you to filter columns by type:ie if DateY/Numy/StringY/Other
typeFn1 :: (FType -> Bool) -> Fn1
typeFn1 p =  map (view _1) . filter (p .view (_2 . _2))

-- | 'elemFn1' allows the user to specify the order and presence of columns for display
elemFn1 :: ([Int] -> [Int]) -> Fn1
elemFn1 p = p . map fst

-- | 'squarble' changes the order in which columns appear depending on a given strategy
squarble :: HasCallStack => Fn1 -> [[([String], FType)]] -> [Int]
squarble fnx rs =
  let iis = map (\r -> fnx (zip [0..length r-1] r)) rs
      ret = foldr1 intersect iis
  in if null ret then error "squarble null" else ret

padMat :: HasCallStack => (a,a) -> [[a]] -> [[a]] -> [[a]]
padMat (ld,rd) xs ys
       | null xs = ys
       | null ys = xs
       | otherwise =
  let lcols = head $ fmap length xs
      rcols = head $ fmap length ys
      f (Just a) (Just b) = a ++ b
      f Nothing (Just b) = replicate lcols ld ++ b
      f (Just a) Nothing = a ++ replicate rcols rd
      f Nothing Nothing = error "impossible case: padMat" -- this cannot happen but it did!
  in A.padZipWith f xs ys

-- cant use semigroup instance for MMM cos rfoldMap expects a monoid
-- could use a semigroup and then wrap in Option to get a Maybe but too much effort
-- | 'MMM' deals with joining resultsets horizontally
newtype MMM a = MMM { unMMM :: [[a]] } deriving (Show,Eq)

instance Monoid a => Monoid (MMM a) where
  mempty = MMM []

-- | special Semigroup instance to jam resultsets together horizontally
instance Monoid a => Semigroup (MMM a) where
  MMM xs <> MMM ys = MMM $ padMat mempty xs ys



emptyMetaColName :: String -> Bool
emptyMetaColName s | all isSpace s = True -- mssql
                   | s == "?column?" = True -- postgres
--                   | anyOf _head isDigit s = True -- numbers: oracle and sqlite
--                   | anyOf _head (=='\'') s = True -- strings: oracle and sqlite
                   | otherwise = False

addMetaToColName :: [String] -> HMeta -> [String]
addMetaToColName xs ys =
  zipWith (\x (y,_) -> if emptyMetaColName y then x else x ++ "(" ++ y ++ ")") xs (ys ++ repeat hMetaNull)

addColumnNameUsingMeta :: Int -> HMeta -> [String]
addColumnNameUsingMeta n ys =
  zipWith (\x (y,_) -> if emptyMetaColName y then x else y) (map (\i -> "col_" <> show i)[1..n]) (ys ++ repeat hMetaNull)

qprttableH :: forall rs . (ReifyConstraint FromField V.Identity rs, RFoldMap rs, HasCallStack)
  => Opts -> [String] -> [Rec V.Identity rs] -> ([ColSpec], [String], MMM [String])
qprttableH _ _ [] = ([upto 20], ["oops"], MMM [[["no data"]]])
qprttableH o colnames ts =
  let ret = ts <&> \w ->
  -- the dictionary is for V.Identity v not v by itself!
                 let aaa = rfoldMap (\(V.Compose (V.Dict v)) -> [(coltype (_oRC o ^. _2) v, convstring o v, fieldtype (v <$ Proxy) v)]) $ reifyConstraint @FromField w
                 in concatMap (\(as,bs,cs) -> SE.zip3Exact as bs cs) aaa
  in case ret of
    zs@(z' : _) ->
      let is = squarble (_oFn1 o) ((map.map) (view _2 &&& view _3) zs)
          cols = map (view _1 . Safe.atNote "qprttableH cols" z') is
      in (cols
         ,map (Safe.atNote "qprttableH flds" colnames) is
         ,MMM (map (\z -> map (view _2 . Safe.atNote "qprttableH MMM" z) is) zs))
    _ -> error "qprttableH empty list!"

qprttableV :: forall rs . (ReifyConstraint FromField V.Identity rs, RFoldMap rs, HasCallStack)
  => Opts -> [String] -> [Rec V.Identity rs] -> String
qprttableV _ _ [] = "*** no data ***" -- could show the columns?
qprttableV o colnames ts =
  let ret = ts <&> \w ->
  -- the dictionary is for V.Identity v not v by itself!
                 let aaa = rfoldMap (\(V.Compose (V.Dict v)) -> [(coltype (_oRC o ^. _2) v, convstring o v, fieldtype (v <$ Proxy) v)]) $ reifyConstraint @FromField w
                 in concatMap (\(as,bs,cs) -> SE.zip3Exact as bs cs) aaa
  in case ret of
        zs@(z' : _) ->
          let is = squarble (_oFn1 o) ((map.map) (view _2 &&& view _3) zs)
              cols = map (view _1 . Safe.atNote "qprttableV cols" z') is
          in tableString cols
             (_oStyle o)
             (titlesH $ map (Safe.atNote ("qprttableV flds " ++ show colnames ++ " is=" ++ show is) colnames) is)
             (map (\z -> colsAllG top (map (view _2 . Safe.atNote "qprttableV colsAllG" z) is)) zs)
        _ -> error "qprttableV empty list!"

-- there is no vertical vs horizontal! cos only one frame!
fprttableRec' :: forall rs t . (Foldable t, ReifyConstraint FromField V.Identity rs, RFoldMap rs, HasCallStack) =>
      Opts -> t (Rec V.Identity rs) -> String
fprttableRec' o (toList -> z@(r1:_)) =
  let len = getSum $ V.rfoldMap (const (Sum @Int 1)) r1
      colnames = map (\i -> '_' : show i) [1 .. len]
      (cs,hs,MMM rs) = qprttableH o colnames (toList z)
  in tableString cs (_oStyle o) (titlesH hs) (map (colsAllG top) rs)
fprttableRec' _ _ = "empty table"

fprttableRecT :: forall (rs :: [(Symbol,*)]) t . (Foldable t, StripFieldNames rs, F.ColumnHeaders rs, ReifyConstraint FromField V.Identity (Unlabeled rs), RFoldMap (Unlabeled rs), HasCallStack) =>
      Opts -> t (F rs) -> String
fprttableRecT o z =
  let colnames = F.columnHeaders (Proxy @(F rs))
      (cs,hs,MMM rs) = qprttableH o colnames (map stripNames (toList z))
  in tableString cs (_oStyle o) (titlesH hs) (map (colsAllG top) rs)
