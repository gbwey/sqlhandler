{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : HSql.Core.TablePrinter
Description : utilities for displaying resultsets in tabular form.
Copyright   : (c) Grant Weyburne, 2016
License     : BSD-3

displays tables in tabular format: 'wprint' is the key function
-}
module HSql.Core.TablePrinter where

import Control.Applicative
import Control.Arrow
import Control.Lens hiding (from)
import Control.Monad.State.Strict
import Data.Bool
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Data.Char (isControl, isSpace, ord)
import Data.Either
import Data.Foldable (toList)
import qualified Data.Functor.Identity as L
import Data.Generics.Product hiding (Rec)
import Data.Generics.Sum
import Data.Kind
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as N
import Data.Pos
import Data.Proxy
import Data.Semigroup
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Text as T
import Data.These
import Data.Time
import Data.Tuple
import Data.Vinyl
import qualified Data.Vinyl as V
import qualified Data.Vinyl.Functor as V
import qualified Data.Vinyl.Recursive as VR
import qualified Data.Vinyl.TypeLevel as V
import Database.HDBC (SqlValue (..))
import Database.HDBC.ColTypes
import DocUtils.Doc
import DocUtils.Generics
import qualified Frames as F
import qualified GHC.Generics as G (Generic)
import GHC.Stack (HasCallStack)
import GHC.TypeLits
import qualified GHC.TypeLits as GL
import qualified Generics.SOP as GS
import HSql.Core.Common
import HSql.Core.Decoder
import HSql.Core.One
import HSql.Core.Operator
import HSql.Core.Sql
import HSql.Core.VinylUtils (F, recLen)
import Numeric (showHex)
import Primus.Error
import Primus.List
import qualified Primus.TypeLevel as TP (FailUnless, LengthT, pnat)
import qualified Safe
import qualified System.IO as SIO
import Text.Layout.Table
import Text.Shakespeare.Text (st)

-- | holds a user defined header or sql metadata
type HDR = These String SqlColDesc

-- | holds metadata state for cells in each row
data CellState = CellState
  { csHeaders :: ![HDR]
  , csPosition :: !Int
  }
  deriving stock (Eq, Show, G.Generic)

-- | holds data needed to render a cell
data Cell = Cell
  { cellValue :: !String
  , cellColSpec :: !ColSpec
  , cellFieldType :: !FType
  , cellHeaderPrefix :: !(Seq (Int, String))
  , cellHeader :: !HeaderSType
  }
  deriving stock (G.Generic)

-- | header types
data HeaderSType
  = SyntheticHeader !String
  | MissingHeaderError !Int
  | RealHeader !HDR
  deriving stock (G.Generic, Show, Eq)

instance Show Cell where
  show cell = psiS (cellValue cell, cellHeaderPrefix cell, cellHeader cell)

-- | intersperse underscores for each header word
underscoreIt :: Opts -> [String] -> String
underscoreIt o xs =
  case filter (not . null) xs of
    [] -> bool mempty "<nada>" (isVerbose o)
    ys@(_ : _) -> L.intercalate "_" ys

-- | differentiates between a real field and synthetic field
data RealField = RealField | SyntheticField !String deriving stock (G.Generic, Show, Eq)

-- | create one or more cells from a field
class FromCell a where
  fromCell ::
    HasCallStack =>
    Opts ->
    Seq (Int, String) ->
    RealField ->
    a ->
    StateT CellState L.Identity (NonEmpty Cell)

-- we could directly pass in this parent info to the child fromCell eg (1,"One")
-- or use csPrefix in cellState
-- we still need the header field
instance FromCell a => FromCell (One a) where
  fromCell o iss lr (One a) = do
    fromCell o (iss |> (1, "One")) lr a

instance
  ( FromCell a
  , KnownNat n
  , TP.FailUnless
      (1 GL.<=? n)
      ( 'GL.Text "FromCell (DecNE n a): requires n >= 1 but found n="
          ':<>: 'GL.ShowType n
          ':$$: 'GL.Text "  a="
          ':<>: 'GL.ShowType a
      )
  ) =>
  FromCell (DecNE n a)
  where
  fromCell o iss lr (DecNE ns) = do
    let n = TP.pnat @n
    -- let hdr1 = hdrPrefix $ cellHeader (N.head ns)
    sconcat <$> mapM (fromCell o (iss |> (1, "DecNE(" ++ show n ++ ")")) lr) ns

instance FromCell a => FromCell (Maybe a) where
  fromCell o iss lr ma = do
    case ma of
      Nothing -> fromCell @String o (iss |> (1, "Nothing")) lr "<null>"
      Just a -> fromCell o (iss |> (1, "Just")) lr a

instance (FromCell a, FromCell b) => FromCell (Either a b) where
  fromCell o iss lr ma = do
    case ma of
      Left a -> fromCell o (iss |> (1, "Left")) lr a
      Right b -> fromCell o (iss |> (1, "Right")) lr b

instance (FromCell t, KnownSymbol s) => FromCell (ElField '(s, t)) where
  fromCell o iss lr (Field v) = fromCell @t o (iss |> (1, symbolVal (Proxy @s))) lr v

instance FromCell a => FromCell (V.Identity a) where
  fromCell o iss lr (V.Identity a) = fromCell o (iss |> (2, "V.Identity")) lr a

instance FromCell a => FromCell (L.Identity a) where
  fromCell o iss lr (L.Identity a) = fromCell o (iss |> (2, "L.Identity")) lr a

instance FromCell [SqlValue] where
  fromCell o iss lr =
    \case
      [] -> normalError "fromCell at instance [SqlValue] is empty"
      x : xs -> sconcat <$> traverse (fromCell o (iss |> (2, "SqlValues")) lr) (x :| xs)

instance FromCell SqlValue where
  fromCell o iss lr v =
    let iss' = iss |> (1, conNameOf v)
     in case v of
          SqlInt32 i -> fromCell @Integer o iss' lr (fromIntegral i)
          SqlBool b -> fromCell o iss' lr b
          SqlInteger i -> fromCell o iss' lr i
          SqlString s -> fromCell o iss' lr s
          SqlByteString bs -> fromCell o iss' lr bs
          SqlInt64 i -> fromCell @Integer o iss' lr (fromIntegral i)
          SqlWord32 i -> fromCell @Integer o iss' lr (fromIntegral i)
          SqlWord64 i -> fromCell @Integer o iss' lr (fromIntegral i)
          SqlChar c -> fromCell o iss' lr c
          SqlDouble i -> fromCell o iss' lr i
          SqlRational r -> fromCell o iss' lr r
          SqlLocalDate day -> fromCell o iss' lr day
          SqlLocalTimeOfDay i -> fromCell o iss' lr i
          SqlZonedLocalTimeOfDay i j -> do
            h <- getHeaderS o lr
            let iss'' =
                  ( case h of
                      RealHeader th -> (|> (1, showHDR o th))
                      MissingHeaderError{} -> id
                      SyntheticHeader{} -> id
                  )
                    iss'
            a <- fromCell o iss'' (SyntheticField "tod") i
            b <- fromCell o iss'' (SyntheticField "tz") j
            return (a <> b)
          SqlLocalTime loc -> fromCell o iss' lr loc
          SqlZonedTime zt -> fromCell o iss' lr zt
          SqlUTCTime u -> fromCell o iss' lr u
          SqlDiffTime nom -> fromCell o iss' lr nom
          SqlPOSIXTime nom -> fromCell o iss' lr nom
          SqlNull -> fromCell @String o iss' lr "<null>"

instance (FromCell a, FromCell b) => FromCell (a, b) where
  fromCell o iss lr (a, b) = liftA2 (<>) (fromCell o (iss |> (3, "TP2")) lr a) (fromCell o (iss |> (3, "TP2")) lr b)
instance (FromCell (a, b), FromCell c) => FromCell (a, b, c) where
  fromCell o iss lr (a, b, c) = liftA2 (<>) (fromCell o (iss |> (3, "TP3")) lr (a, b)) (fromCell o (iss |> (3, "TP3")) lr c)
instance (FromCell (a, b, c), FromCell d) => FromCell (a, b, c, d) where
  fromCell o iss lr (a, b, c, d) = liftA2 (<>) (fromCell o (iss |> (3, "TP4")) lr (a, b, c)) (fromCell o (iss |> (3, "TP4")) lr d)
instance (FromCell (a, b, c, d), FromCell e) => FromCell (a, b, c, d, e) where
  fromCell o iss lr (a, b, c, d, e) = liftA2 (<>) (fromCell o (iss |> (3, "TP5")) lr (a, b, c, d)) (fromCell o (iss |> (3, "TP5")) lr e)
instance (FromCell (a, b, c, d, e), FromCell f) => FromCell (a, b, c, d, e, f) where
  fromCell o iss lr (a, b, c, d, e, f) = liftA2 (<>) (fromCell o (iss |> (3, "TP6")) lr (a, b, c, d, e)) (fromCell o (iss |> (3, "TP6")) lr f)
instance (FromCell (a, b, c, d, e, f), FromCell g) => FromCell (a, b, c, d, e, f, g) where
  fromCell o iss lr (a, b, c, d, e, f, g) = liftA2 (<>) (fromCell o (iss |> (3, "TP7")) lr (a, b, c, d, e, f)) (fromCell o (iss |> (3, "TP7")) lr g)
instance (FromCell (a, b, c, d, e, f, g), FromCell h) => FromCell (a, b, c, d, e, f, g, h) where
  fromCell o iss lr (a, b, c, d, e, f, g, h) = liftA2 (<>) (fromCell o (iss |> (3, "TP8")) lr (a, b, c, d, e, f, g)) (fromCell o (iss |> (3, "TP8")) lr h)
instance (FromCell (a, b, c, d, e, f, g, h), FromCell i) => FromCell (a, b, c, d, e, f, g, h, i) where
  fromCell o iss lr (a, b, c, d, e, f, g, h, i) = liftA2 (<>) (fromCell o (iss |> (3, "TP9")) lr (a, b, c, d, e, f, g, h)) (fromCell o (iss |> (3, "TP9")) lr i)
instance (FromCell (a, b, c, d, e, f, g, h, i), FromCell j) => FromCell (a, b, c, d, e, f, g, h, i, j) where
  fromCell o iss lr (a, b, c, d, e, f, g, h, i, j) = liftA2 (<>) (fromCell o (iss |> (3, "TP10")) lr (a, b, c, d, e, f, g, h, i)) (fromCell o (iss |> (3, "TP10")) lr j)

instance FromCell NominalDiffTime where
  fromCell o iss lr = fromCell o (iss |> (2, "NominalDiffTime")) lr . show
instance FromCell TimeOfDay where
  fromCell o iss lr = fromCell o (iss |> (2, "TimeOfDay")) lr . show
instance FromCell Rational where
  fromCell o iss lr = fromCell o (iss |> (2, "Rational")) lr . show
instance FromCell Bool where
  fromCell o iss lr = fromCell o (iss |> (2, "Bool")) lr . show
instance FromCell Char where
  fromCell o iss lr c = do
    hdr <- getHeaderS o lr
    let val = case hdr of
          RealHeader (That SqlColDesc{colType = SqlTinyIntT}) ->
            case c of
              '\0' -> "false"
              '\1' -> "true"
              oops -> normalError $ "invalid state for boolean!! " ++ show oops
          _nometadata -> [c]
    return $ pure $ Cell val (upto (_P @5)) Stringy (iss |> (2, "Char")) hdr

instance FromCell Int where
  fromCell o iss lr a = do
    hdr <- getHeaderS o lr
    return $ pure $ Cell (show a) numCol Numy (iss |> (2, "Int")) hdr
instance FromCell Integer where
  fromCell o iss lr a = do
    hdr <- getHeaderS o lr
    return $ pure $ Cell (show a) numCol Numy (iss |> (2, "Integer")) hdr
instance FromCell Float where
  fromCell o iss lr a = do
    hdr <- getHeaderS o lr
    return $ pure $ Cell (show a) numCol Numy (iss |> (2, "Float")) hdr
instance FromCell Double where
  fromCell o iss lr a = do
    hdr <- getHeaderS o lr
    return $ pure $ Cell (show a) numCol Numy (iss |> (2, "Double")) hdr
instance FromCell String where
  fromCell o iss lr a = do
    hdr <- getHeaderS o lr
    return $ pure $ Cell a (upto (oRC o ^. _2)) Stringy (iss |> (2, "String")) hdr
instance FromCell Text where
  fromCell o iss lr a = do
    hdr <- getHeaderS o lr
    return $ pure $ Cell (T.unpack a) (upto (oRC o ^. _2)) Stringy (iss |> (2, "Text")) hdr

instance FromCell ByteString where
  fromCell o iss lr a = do
    hdr <- getHeaderS o lr
    return $ pure $ Cell (B8.unpack a) (upto (oRC o ^. _2)) Stringy (iss |> (2, "ByteString")) hdr

instance FromCell UTCTime where
  fromCell o iss lr a = do
    hdr <- getHeaderS o lr
    return $ pure $ Cell (formatTime defaultTimeLocale "%F %T" a) numCol Datey (iss |> (2, "UTCTime")) hdr

instance FromCell ZonedTime where
  fromCell o iss lr a = do
    hdr <- getHeaderS o lr
    return $ pure $ Cell (formatTime defaultTimeLocale "%F %T" a) numCol Datey (iss |> (2, "ZonedTime")) hdr

instance FromCell LocalTime where
  fromCell o iss lr a = do
    hdr <- getHeaderS o lr
    return $ pure $ Cell (formatTime defaultTimeLocale "%F %T" a) numCol Datey (iss |> (2, "LocalTime")) hdr

instance FromCell Day where
  fromCell o iss lr a = do
    hdr <- getHeaderS o lr
    return $ pure $ Cell (formatTime defaultTimeLocale "%F" a) numCol Datey (iss |> (2, "Day")) hdr

instance FromCell TimeZone where
  fromCell o iss lr tz = do
    h <- getHeaderS o lr
    let hdrx = showHeaderSType o h
    let iss' = iss |> (1, "TimeZone") |> (2, "[" <> hdrx <> "]")
    a <- fromCell o iss' (SyntheticField "minutes") (timeZoneMinutes tz)
    b <- fromCell o iss' (SyntheticField "summeronly") (timeZoneSummerOnly tz)
    c <- fromCell o iss' (SyntheticField "name") (timeZoneName tz)
    return (a <> b <> c)

{-
-- needs PolyKinds else 'True for p won't work! W 'True will work cos is kind Type
instance FromCell a => FromCell (Refined opts p a) where
  fromCell o iss lr = fromCell o (iss |> (1, "Refined")) lr . unRefined

instance (FromCell i, FromCell (PP ip i)) => FromCell (Refined2 opts ip op i) where
  fromCell o iss lr r = do
    x <- fromCell o (iss |> (1, "Refined2In")) lr (r2In r)
    y <- fromCell o (iss |> (1, "Refined2Out")) (SyntheticField "r2Out") (r2Out r)
    return (x <> y)

instance (FromCell i, FromCell (PP ip i)) => FromCell (Refined3 opts ip op fmt i) where
  fromCell o iss lr r = do
    x <- fromCell o (iss |> (1, "Refined3In")) lr (r3In r)
    y <- fromCell o (iss |> (1, "Refined3Out")) (SyntheticField "r3Out") (r3Out r)
    return (x <> y)
-}

-- | is the verbose option set
isVerbose :: Opts -> Bool
isVerbose = (> 0) . oVerbose

-- | extract the headers from cellstate and combine with the real header
askHeaderS :: Opts -> RealField -> CellState -> HeaderSType
askHeaderS o lr cs =
  case lr of
    SyntheticField fldname -> do
      SyntheticHeader
        ("@" <> underscoreIt o (fldname : bool mempty [show (1 + csPosition cs)] (isVerbose o)))
    RealField -> do
      case csHeaders cs of
        [] -> MissingHeaderError (1 + csPosition cs)
        h : _ -> RealHeader h

-- | extracts a header from 'CellState'
getHeaderS :: Monad m => Opts -> RealField -> StateT CellState m HeaderSType
getHeaderS o lr = do
  cs <- get
  let ret = askHeaderS o lr cs
  modify (\s -> s{csPosition = csPosition s + 1})
  case (ret, csHeaders cs) of
    (RealHeader{}, _ : hs) -> modify (\s -> s{csHeaders = hs})
    (RealHeader{}, []) -> programmError $ "how does this happen " ++ psiS cs
    _nometadata -> return ()
  return ret

-- | display cell header
showHeaderS :: Opts -> Cell -> String
showHeaderS o cell =
  let hdr1 = L.intercalate "_" $ filter (not . null . trim) $ map snd $ filter ((<= oVerbose o) . fst) $ toList $ cellHeaderPrefix cell
      hdr2 = showHeaderSType o (cellHeader cell)
   in {-
            g txt = bool mempty txt (isVerbose o)
            hdr2 = case cellHeader cell of
              SyntheticHeader s -> g "%" <> s
              RealHeader th -> case th of
                This a -> g "@" <> a
                That m -> g "*" <> underscoreIt o (colName m : bool mempty [showMeta False m] (oVerbose o > 1))
                These a m -> g "#" <> underscoreIt o (a : colName m : bool mempty [showMeta False m] (oVerbose o > 1))
              MissingHeaderError i -> "!MISSING!_" <> show i -- error: user did not provide enough headers
      -}
      underscoreIt o [hdr1, hdr2]

-- | show header and header type
showHeaderSType :: Opts -> HeaderSType -> String
showHeaderSType o h =
  let g txt = bool mempty txt (oVerbose o > 2)
   in case h of
        SyntheticHeader s -> g "[Synth]" <> s
        MissingHeaderError i -> g ("[MISSING!" <> show i <> "]")
        RealHeader th -> showHDR o th

-- | show header
showHDR :: Opts -> HDR -> String
showHDR o th =
  let f v = underscoreIt o (colName v : bool mempty [showMeta False v] (oVerbose o > 1))
      g txt = bool mempty txt (oVerbose o > 2)
   in case th of
        This s -> g "[This]" <> s
        That v -> g "[That]" <> f v
        These s v -> g "[These]" <> s <> "_" <> f v

-- | create cells for a record
toRow ::
  forall a xs.
  ( GS.Generic a
  , GS.HasDatatypeInfo a
  , GS.Code a ~ '[xs] -- this is ambiguous when using a singleton
  , GS.All FromCell xs
  , HasCallStack
  ) =>
  Opts ->
  Seq (Int, String) ->
  a ->
  StateT CellState L.Identity (NonEmpty Cell)
toRow o iss a =
  case GS.from a of
    GS.SOP (GS.Z xs) ->
      let ret =
            GS.hcollapse
              ( GS.hcliftA
                  ff
                  ( \(GS.I v) ->
                      GS.K
                        ( fromCell o iss RealField v
                        )
                  )
                  xs
              )
       in sequenceA ret <&> \case
            [] -> normalError "toRow: empty row found" -- todo: can this ever happen?
            b : bs -> sconcat (b :| bs)
    GS.SOP (GS.S _) -> programmError "impossible case: toRow SOP S"
 where
  ff = Proxy :: Proxy FromCell

-- | print a table vertically
prttableV ::
  forall xs a.
  ( GS.HasDatatypeInfo a
  , GS.Code a ~ '[xs]
  , GS.All FromCell xs
  ) =>
  Opts ->
  RMeta ->
  [a] ->
  String
prttableV o meta as =
  case prttableImpl o meta as of
    Nothing -> "*** no data ***"
    Just (colspecs, hdrs, rows) ->
      tableString
        colspecs
        (oStyle o)
        (titlesH hdrs)
        (map (colsAllG top) rows)

-- | internal for printing a table
prttableImpl ::
  forall xs a.
  ( GS.HasDatatypeInfo a
  , GS.Code a ~ '[xs]
  , GS.All FromCell xs
  ) =>
  Opts ->
  RMeta ->
  [a] ->
  Maybe ([ColSpec], [String], [[[String]]])
prttableImpl o meta =
  \case
    [] -> Nothing
    t : ts ->
      let hdrs = getHdrFields @a Proxy meta
          xxs = N.map (\x -> runState (toRow o mempty x) (CellState hdrs 0)) (t :| ts)
          (colspecs, hdrsNEW, coldata) = lastStep o (N.map fst xxs)
       in Just (colspecs, hdrsNEW, coldata)

-- | get header fields for a record
getHdrFields ::
  forall a.
  GS.HasDatatypeInfo a =>
  Proxy a ->
  RMeta ->
  [HDR]
getHdrFields _ =
  zipLongest (map trim (fromRight [] (getFieldNames (Proxy @a))))

-- | a type synonym for functions that change the order of columns displayed or even drop columns
newtype Fn1 = Fn1 {unFn1 :: [(Int, ([String], (FType, HeaderSType)))] -> [Int]}

-- | finds the intersecting columns of two 'Fn1'
joinFn1 :: Fn1 -> Fn1 -> Fn1
joinFn1 (Fn1 f) (Fn1 g) = Fn1 $ liftA2 L.intersect f g

-- | finds the intersecting columns of many 'Fn1'
joinFn1s :: [Fn1] -> Maybe Fn1
joinFn1s = \case
  [] -> Nothing
  x : xs -> Just $ foldr1 joinFn1 (x :| xs)

instance Show Fn1 where
  show (Fn1 _) = "Fn1<fn>"

-- | coarse classification of the type of data in a cell
data FType
  = Stringy
  | Numy
  | Datey
  | Other
  deriving stock (Show, Eq, Bounded, Enum, Ord, G.Generic)

-- | how to handle data that is wider than a cell
data FixData
  = FWrap
  | FTrunc
  deriving stock (Eq, Show, G.Generic)

-- | type synonym used for functions that transform the text of a cell
newtype Morph = Morph {unMorph :: String -> String}

instance Show Morph where
  show Morph{} = "Morph<fn>"

-- | 'Opts' these are the options for displaying resultsets see 'wprintWith'
data Opts = Opts
  { oFile :: !(Maybe FilePath)
  -- ^ optionally print to a file
  , oStyle :: !TableStyle
  -- ^ specify style eg unicode ascii etc
  , oFixData :: !FixData
  , oFn1 :: !(Maybe Fn1)
  -- ^ change the order and number of columns to display
  , oRC :: !(Pos, Pos)
  -- ^ default row and column size of each cell
  , oMorph :: !Morph
  -- ^ transform the text of a cell: used for handling control characters
  , oMaxSizeFields :: !(Maybe Pos)
  -- ^ filter out fields that are bigger than the given size and return the sql
  , oVerbose :: !Int
  }
  deriving stock (G.Generic)

instance Show Opts where
  show o =
    "Opts:"
      ++ " oFile = "
      ++ show (oFile o)
      ++ " oFixData = "
      ++ show (oFixData o)
      ++ " oRC = "
      ++ show (oRC o)
      ++ " oMaxSizeFields = "
      ++ show (oMaxSizeFields o)
      ++ " oVerbose = "
      ++ show (oVerbose o)

-- | 'defT' default options
defT :: Opts
defT =
  Opts
    { oFile = Nothing
    , oStyle = unicodeS
    , oFixData = FTrunc
    , oFn1 = Nothing
    , oRC = (_P @5, _P @50)
    , oMorph = defMorph1
    , oMaxSizeFields = Nothing
    , oVerbose = 0
    }

-- | default options but allow some common customisation
defTSimple :: FixData -> Pos -> Pos -> Morph -> Opts
defTSimple xd r c morph =
  Opts
    { oFile = Nothing
    , oStyle = unicodeS
    , oFixData = xd
    , oFn1 = Nothing
    , oRC = (r, c)
    , oMorph = morph
    , oMaxSizeFields = Nothing
    , oVerbose = 0
    }

-- | set the table style option
poStyle :: TableStyle -> Opts -> Opts
poStyle ts o = o{oStyle = ts}

-- | set the max field size option
poMaxSizeFields :: Pos -> Opts -> Opts
poMaxSizeFields i o = o{oMaxSizeFields = Just i}

-- | set the 'Morph' option
poMorph :: Morph -> Opts -> Opts
poMorph m o = o{oMorph = m}

-- | 'poRC' allows you to adjust the max number of subrows and columns within a cell
poRC :: ((Pos, Pos) -> (Pos, Pos)) -> Opts -> Opts
poRC f opts = opts{oRC = f (oRC opts)}

-- | 'poR' allows you to adjust the max number of subrows within a cell
poR :: (Pos -> Pos) -> Opts -> Opts
poR f opts = opts{oRC = first f (oRC opts)}

-- | 'poC' allows you to adjust the max columns size with a cell
poC :: (Pos -> Pos) -> Opts -> Opts
poC f opts = opts{oRC = second f (oRC opts)}

-- | 'poCols' allows you to change which columns get shown
poCols :: ([Int] -> [Int]) -> Opts -> Opts
poCols fn o = o{oFn1 = Just $ Fn1 $ fn . map fst}

-- | 'poFile' allows you send the output to a file
poFile :: FilePath -> Opts -> Opts
poFile fn o = o{oFile = Just fn}

-- | 'poTrunc' truncates the data in each cell if it is too large
poTrunc :: Opts -> Opts
poTrunc o = o{oFixData = FTrunc}

-- | 'poWrap' wraps the data in each cell if it is too large
poWrap :: Opts -> Opts
poWrap o = o{oFixData = FWrap}

-- | uses 'asciiS' table style instead of unicode (on windows is useful when writing to a file)
poAscii :: Opts -> Opts
poAscii o = o{oStyle = asciiS}

-- | uses 'asciiRoundS' table style
poAsciiRound :: Opts -> Opts
poAsciiRound o = o{oStyle = asciiRoundS}

-- | uses 'asciiDoubleS' table style
poAsciiDouble :: Opts -> Opts
poAsciiDouble o = o{oStyle = asciiDoubleS}

-- | uses 'unicodeS' table style
poUnicode :: Opts -> Opts
poUnicode o = o{oStyle = unicodeS}

-- | uses 'unicodeRoundS' table style
poUnicodeRound :: Opts -> Opts
poUnicodeRound o = o{oStyle = unicodeRoundS}

-- | uses 'unicodeBoldHeaderS' table style
poUnicodeBoldHeader :: Opts -> Opts
poUnicodeBoldHeader o = o{oStyle = unicodeBoldHeaderS}

-- | uses 'unicodeBoldS' table style
poUnicodeBold :: Opts -> Opts
poUnicodeBold o = o{oStyle = unicodeBoldS}

-- | uses 'unicodeBoldStripedS' table style
poUnicodeBoldStriped :: Opts -> Opts
poUnicodeBoldStriped o = o{oStyle = unicodeBoldStripedS}

-- | uses 'unicodeDoubleFrameS' table style
poUnicodeDoubleFrame :: Opts -> Opts
poUnicodeDoubleFrame o = o{oStyle = unicodeDoubleFrameS}

-- | choose a style based on enumeration
poStyleInt :: Int -> Opts -> Opts
poStyleInt i o =
  case Safe.atMay tableStyles i of
    Nothing -> normalError $ "poStyleInt: out of bounds: found " ++ show i ++ " but expected [0.." ++ show mx ++ "]"
    Just (_, ts) -> o{oStyle = ts}
 where
  mx = length tableStyles - 1

-- | all the possible 'TableStyle's
tableStyles :: [(String, TableStyle)]
tableStyles =
  [ ("asciiRoundS", asciiRoundS)
  , ("asciiS", asciiS)
  , ("asciiDoubleS", asciiDoubleS)
  , ("unicodeS", unicodeS)
  , ("unicodeBoldHeaderS", unicodeBoldHeaderS)
  , ("unicodeRoundS", unicodeRoundS)
  , ("unicodeBoldS", unicodeBoldS)
  , ("unicodeBoldStripedS", unicodeBoldStripedS)
  , ("unicodeDoubleFrameS", unicodeDoubleFrameS)
  ]

-- | set the verbose level
poVerbose :: Int -> Opts -> Opts
poVerbose i o = o{oVerbose = i}

-- | set to verbose level one
poV1 :: Opts -> Opts
poV1 = poVerbose 1

-- | set to verbose level two
poV2 :: Opts -> Opts
poV2 = poVerbose 2

-- | set to verbose level three
poV3 :: Opts -> Opts
poV3 = poVerbose 3

-- | set the 'Fn1' option
poFn1 :: Fn1 -> Opts -> Opts
poFn1 f o = o{oFn1 = Just f}

-- | convert a character in hex
hexChar :: Char -> String
hexChar !c =
  let (a, b) = quotRem (ord c) 16
   in showHex a (showHex b "")

-- | prints resultsets as they appear (vertically)
prttableRecV ::
  (HasCallStack, V.RecAll RState rs ZPrint) =>
  Opts ->
  Rec RState rs ->
  String
prttableRecV o xs =
  let ret = VR.reifyConstraint (Proxy @ZPrint) xs
   in VR.rfoldMap V.getConst $
        fst $
          flip runState (0 :: Int) $
            V.rtraverse
              (\(V.Compose (V.Dict x)) -> state $ \pos -> swap $ fmap V.Const (zprintV x o rsHeader1 pos))
              ret

-- | describes the information needed for the resultset header
type FnRSHeader = (String, Int) -> String

-- | prefix a 'FnRSHeader'
prefixMessage :: FnRSHeader -> String -> FnRSHeader
prefixMessage k s (ss, ii) = k (s <> ss, ii)

-- | display position of the result
rsHeader1 :: FnRSHeader
rsHeader1 (s, i) = "\nresultset " <> show (i + 1) <> " " <> s <> "\n"

-- | skip position of the result
rsHeader2 :: FnRSHeader
rsHeader2 = const "\n"

-- | pretty print meta data
dumpMeta :: Show a => Opts -> a -> String
dumpMeta o meta
  | oVerbose o > 0 = "\n" <> psiS meta <> "\n"
  | otherwise = ""

-- | add a single message to 'MMM'
nl2 :: String -> MMM
nl2 = MMM . pure . pure . pure

-- | get list of field names for a record or if no field names return the number of fields
getFieldNames :: GS.HasDatatypeInfo a => proxy a -> Either Int [String]
getFieldNames = fNms . GS.constructorInfo . GS.datatypeInfo

-- | get list of field names for a record or if no field names return the number of fields
fNms :: HasCallStack => GS.NP GS.ConstructorInfo a -> Either Int [String]
fNms (GS.Record _ xs GS.:* _) = Right (fNmsRec xs)
fNms ((GS.Constructor _ :: GS.ConstructorInfo z) GS.:* _) =
  Left (GS.lengthSList (Proxy @z))
fNms GS.Nil = programmError "impossible case: fNms Nil"
fNms (GS.Infix{} GS.:* _) = programmError "impossible case: fNms Infix"

-- | get list of field names for a record
fNmsRec :: GS.NP GS.FieldInfo a -> [String]
fNmsRec GS.Nil = []
fNmsRec (GS.FieldInfo nm GS.:* rest) = nm : fNmsRec rest

-- | set the maximum column size
upto :: Pos -> ColSpec
upto (Pos i) = column (expandUntil i) Text.Layout.Table.left def def

-- | different ways to render the output: ie how to handle newlines
defMorph1 :: Morph
defMorph1 = morph1 ("\n", "", " ") (morphFn1 False)

-- | different ways to render the output: ie how to handle newlines
defMorph2 :: Morph
defMorph2 = morph1 ("\n", "\\r", "\\t") (morphFn1 False)

-- | different ways to render the output: ie how to handle newlines
defMorph3 :: Morph
defMorph3 = morph1 (" ", "\\r", "\\t") (morphFn1 False)

-- | different ways to render the output: ie how to handle newlines
defMorph4 :: Morph
defMorph4 = morph1 ("\\n", "\\r", "\\t") (morphFn1 False)

-- | different ways to render the output: ie how to handle newlines
defMorph5 :: Morph
defMorph5 = morph1 ("\\n", "\\r", "\\t") (morphFn1 True)

-- | different ways to render the output: ie how to handle newlines
defMorph6 :: Morph
defMorph6 = morph1 ("\n", "\\r", "\\t") (morphFn1 True)

-- | hide or dump out control characters
morphFn1 :: Bool -> Char -> String
morphFn1 hideControlChars c
  | hideControlChars = []
  | otherwise = "?" <> hexChar c <> "?"

-- | replace control chars
morph1 :: (String, String, String) -> (Char -> String) -> Morph
morph1 (controlN, controlR, controlT) fn =
  Morph $
    concatMap
      ( \case
          '\n' -> controlN
          '\r' -> controlR
          '\t' -> controlT
          c | isControl c -> fn c
          o -> [o]
      )

-- | flattens a cell for display based on 'Opts'
flattenCell :: Opts -> [String] -> [String]
flattenCell o s1 =
  let (Pos r, Pos c) = oRC o
      s2 = case oFixData o of
        FWrap -> concatMap (chunksOf (snd (oRC o))) s1
        FTrunc -> map (take (c + 1)) s1 -- set upto to value of c
      s3 = take r s2
   in if length s2 > r then s3 & _Snoc . _2 %~ (<> "~") else s3

-- | display a single cell
prttableVCol ::
  forall a.
  FromCell a =>
  Opts ->
  RMeta ->
  [a] ->
  String
prttableVCol o meta =
  \case
    [] -> "*** no data ***"
    -- dont traverse/sequence because we want to run with the same headers for each row else will run out of headers after 1st row
    t : ts ->
      let xxs = N.map (\x -> runState (fromCell o mempty RealField x) (CellState (map That meta) 0)) (t :| ts)
          (colspecs, hdrsNEW, coldata) = lastStep o (N.map fst xxs)
       in tableString
            colspecs
            (oStyle o)
            (titlesH hdrsNEW)
            (map (colsAllG top) coldata)

-- | internal method for extracting the column specs, headers, and data
lastStep ::
  HasCallStack =>
  Opts ->
  NonEmpty (NonEmpty Cell) ->
  ([ColSpec], [String], [[[String]]])
lastStep o cells =
  let rs = maybe id reindexTable (oFn1 o) cells
      colspecs = N.map cellColSpec (N.head rs)
      hdrs = N.map (showHeaderS o) (N.head rs)
      coldata = N.toList $ (N.map . N.map) (flattenCell o . lines . unMorph (oMorph o) . cellValue) rs
   in (N.toList colspecs, N.toList hdrs, N.toList <$> coldata)

-- | changes the order in which columns appear depending on a given strategy
reindexTable :: HasCallStack => Fn1 -> NonEmpty (NonEmpty Cell) -> NonEmpty (NonEmpty Cell)
reindexTable fn1 css =
  let iis = reindexTableImpl fn1 $ (fmap . fmap) (pure . cellValue &&& cellFieldType &&& cellHeader) css
   in css <&> \cs -> N.map (atNoteL "reindexTable" (N.toList cs)) iis

-- | determines the order in which columns based on 'Fn1'
reindexTableImpl ::
  HasCallStack =>
  Fn1 ->
  NonEmpty (NonEmpty ([String], (FType, HeaderSType))) ->
  NonEmpty Int
reindexTableImpl fn css =
  let iis = N.map (unFn1 fn . zip [0 ..] . N.toList) css
      numcols = length (N.head css)
   in case N.filter (not . null) $ N.map (filter (>= numcols)) iis of
        [] ->
          fromList1 "reindexTableImpl: no data after intersect" $
            foldr1 L.intersect iis
        errindexes@(_ : _) -> normalError $ "reindexTableImpl:out of bounds: numcols=" <> show numcols <> " errindexes=\n" <> psiS errindexes

-- | associates a sql column type with a 'ColSpec'
whatcoltype :: Pos -> SqlTypeId -> ColSpec
whatcoltype i x
  | x `elem` [SqlCharT, SqlVarCharT, SqlLongVarCharT, SqlWCharT, SqlWVarCharT, SqlWLongVarCharT] =
      upto i
  | x `elem` [SqlDecimalT, SqlNumericT, SqlSmallIntT, SqlIntegerT, SqlRealT, SqlFloatT, SqlDoubleT, SqlBitT, SqlTinyIntT, SqlBigIntT] =
      numCol
  | x `elem` [SqlBinaryT, SqlVarBinaryT, SqlLongVarBinaryT] =
      upto i
  | x `elem` [SqlDateT, SqlTimeT, SqlTimeWithZoneT, SqlTimestampT, SqlTimestampWithZoneT, SqlUTCDateTimeT, SqlUTCTimeT] =
      def
  | x == SqlGUIDT =
      def
  | otherwise = def

-- varchar(max) show as SqlVarCharT with length Just 0! so have to special case for all string types

-- | creates a select statement that restricts the size of the columns to a maximum of "mmx"
getMssqlFromMeta :: Maybe Pos -> RMeta -> Text
getMssqlFromMeta mmx meta =
  let ret = case mmx of
        Nothing -> map (T.pack . colName) meta
        Just (Pos mx) ->
          let (as, bs) = L.partition (\m -> Just mx >= colSize m || (colSize m == Just 0 && colType m `elem` [SqlVarCharT, SqlLongVarCharT, SqlWCharT, SqlWVarCharT, SqlWLongVarCharT, SqlBinaryT, SqlVarBinaryT, SqlLongVarBinaryT])) meta
              ln1 = map (T.pack . colName) as
              ln2 = map ((\nm -> [st|convert(varchar(#{mx}),substring(#{nm},1,#{mx})) as #{nm}|]) . colName) bs
           in ln1 <> ln2
   in T.intercalate "," ret

-- | print resultsets concatenated horizontally
prtHRetCol :: Opts -> [ResultSet] -> [String]
prtHRetCol o =
  \case
    [] -> ["nothing to print"]
    lrs@(_ : _) ->
      let prefix i = "\n" <> show i <> " of " <> show (length lrs)
       in zip [1 :: Int ..] lrs <&> \case
            (i, Left rc) -> prefix i <> " Update rc=" <> show rc
            (i, Right (meta, rss')) ->
              let zz = T.unpack (getMssqlFromMeta (oMaxSizeFields o) meta <> "\n")
               in "fieldnames: " <> zz <> case rss' of
                    [] ->
                      prefix i <> "\nNo Data found so showing column types from metadata\n"
                        <> tableString @String
                          (replicate (length meta) (upto (_P @30)))
                          (oStyle o)
                          (titlesH (map colName meta))
                          [colsAllG top (map (pure . showMeta True) meta)]
                    rs : rss ->
                      let cells = N.map (fromCell o mempty RealField) (rs :| rss)
                          xxs = N.map (\x -> runState x (CellState (map That meta) 0)) cells
                          (colspecs, hdrsNEW, coldata) = lastStep o (N.map fst xxs)
                       in prefix i <> "\n" <> tableString colspecs (oStyle o) (titlesH hdrsNEW) (map (colsAllG top) coldata)

-- | extract the constructor names of each column
getColumnTypes :: [[SqlValue]] -> [String]
getColumnTypes rrs =
  let nn = show SqlNull
   in L.transpose rrs <&> \cs ->
        let ss = map conNameOf cs
         in drop 3 $ Safe.headDef nn (dropWhile (== nn) ss)

-- | 'Fn1' strategy to restrict the columns displayed
colFn1 :: ([Int] -> [Int]) -> Fn1
colFn1 fn = Fn1 $ fn . map fst

-- | select only certain column based on the Header
colTypeFn1 :: (HeaderSType -> Bool) -> Fn1
colTypeFn1 p = Fn1 $ map fst . filter (p . view (_2 . _2 . _2))

-- | match on sql metadata
colDescFn1Pred :: Bool -> (SqlColDesc -> Bool) -> (HeaderSType -> Bool)
colDescFn1Pred b p =
  maybe b p . preview (_Ctor @"RealHeader" . param @0)

-- | exclude all columns with a size over "n"
limitColSizeFn1 :: Pos -> Fn1
limitColSizeFn1 (Pos n) = colTypeFn1 (colDescFn1Pred True ((<= Just n) . colSize))

-- | excludes any column where the length of the sum of all the strings >=i
lengthFn1 :: Pos -> Fn1
lengthFn1 (Pos i) = Fn1 $ map (view _1) . filter ((< i) . length . concat . view (_2 . _1))

-- | exclude columns if any field is over a certain size
lengthAnyFn1 :: Pos -> Fn1
lengthAnyFn1 (Pos i) = Fn1 $ map (view _1) . filter ((< i) . Safe.maximumBound 0 . map length . view (_2 . _1))

-- | 'typeFn1' allows you to filter columns by type:ie if DateY/Numy/StringY/Other
typeFn1 :: (FType -> Bool) -> Fn1
typeFn1 p = Fn1 $ map (view _1) . filter (p . view (_2 . _2 . _1))

-- | combine two matrices padding them to have the same number of rows using defaults
padMat :: (a, a) -> [[a]] -> [[a]] -> [[a]]
padMat (ld, rd) xs ys =
  case (xs, ys) of
    ([], _) -> ys
    (_, []) -> xs
    (x : _, y : _) ->
      let lcols = length x
          rcols = length y
          f (a : as) (b : bs) = (a ++ b) : f as bs
          f [] bs = map (\b -> replicate lcols ld ++ b) bs
          f as [] = map (\a -> a ++ replicate rcols rd) as
       in f xs ys

-- | holds column specs, column headers, and data
data CSMS = CSMS
  { csColSpecs :: ![ColSpec]
  , csCols :: ![String]
  , csMMM :: !MMM
  }

instance Semigroup CSMS where
  CSMS a b c <> CSMS a1 b1 c1 = CSMS (a <> a1) (b <> b1) (c <> c1)

instance Monoid CSMS where
  mempty = CSMS mempty mempty mempty

-- cant use semigroup instance for MMM cos rfoldMap expects a monoid
-- could use a semigroup and then wrap in Option to get a Maybe but too much effort

-- | 'MMM' deals with joining resultsets horizontally
newtype MMM = MMM [[[String]]]
  deriving stock (Show, Eq)

-- | special Semigroup instance to jam resultsets together horizontally
instance Semigroup MMM where
  MMM xs <> MMM ys = MMM $ padMat mempty xs ys

instance Monoid MMM where
  mempty = MMM mempty

-- | determines if a column header is not defined
emptyMetaColName :: String -> Bool
emptyMetaColName s
  | all isSpace s = True -- mssql
  | s == "?column?" = True -- postgres
  --                   | anyOf _head isDigit s = True -- numbers: oracle and sqlite
  --                   | anyOf _head (=='\'') s = True -- strings: oracle and sqlite
  | otherwise = False

-- | append headers and sql metadata
addMetaToColName :: [String] -> RMeta -> [These String SqlColDesc]
addMetaToColName = zipLongest

-- | append unknown columns if not enough sql metadata
addColumnNameUsingMeta :: Int -> RMeta -> [These String SqlColDesc]
addColumnNameUsingMeta n rs =
  map That rs
    <> map (\i -> This $ "<unknown" ++ show i ++ ">") [length rs .. n]

-- | used by 'qprttableV' to display a resultset
qprttableImpl ::
  forall rs.
  ( ReifyConstraint FromCell V.Identity rs
  , RFoldMap rs
  ) =>
  Opts ->
  [Rec V.Identity rs] ->
  Maybe (StateT CellState L.Identity (NonEmpty (NonEmpty Cell)))
qprttableImpl o =
  \case
    [] -> Nothing
    z : zs ->
      let ret = z :| zs <&> rfoldMap (\(V.Compose (V.Dict v)) -> [fromCell o mempty RealField v]) . reifyConstraint @FromCell
       in Just $ sequenceA $ sconcat $ fromList1 "qprttableImpl" <$> ret

-- | used by 'fprttableRec'' to print a frame of data
qprttableH ::
  forall rs.
  ( ReifyConstraint FromCell V.Identity rs
  , RFoldMap rs
  , HasCallStack
  ) =>
  Opts ->
  [String] ->
  [Rec V.Identity rs] ->
  CSMS
qprttableH o hdrs as =
  case qprttableImpl o as of
    Nothing -> CSMS [upto (_P @20)] ["no data"] (nl2 "no data")
    Just cellstate ->
      let xxs = runState cellstate (CellState (map This hdrs) 0)
          (colspecs, hdrsNEW, coldata) = lastStep o (fst xxs)
       in CSMS colspecs hdrsNEW (MMM coldata)

-- | display a resultset at a time
qprttableV ::
  forall rs.
  (ReifyConstraint FromCell V.Identity rs, RFoldMap rs, HasCallStack) =>
  Opts ->
  [HDR] ->
  [Rec V.Identity rs] ->
  String
qprttableV o hdrs as =
  case qprttableImpl o as of
    Nothing -> "*** no data ***" -- could show the columns
    Just cellstate ->
      let xxs = runState cellstate (CellState hdrs 0)
          (colspecs, hdrsNEW, coldata) = lastStep o (fst xxs)
       in tableString colspecs (oStyle o) (titlesH hdrsNEW) (map (colsAllG top) coldata)

-- | used by 'wfn' to print a frame of data
fprttableRecT ::
  forall (rs :: [(Symbol, Type)]) t.
  ( Foldable t
  , StripFieldNames rs
  , F.ColumnHeaders rs
  , ReifyConstraint FromCell V.Identity (Unlabeled rs)
  , RFoldMap (Unlabeled rs)
  , HasCallStack
  ) =>
  Opts ->
  t (F rs) ->
  String
fprttableRecT o z =
  let colnames = F.columnHeaders (Proxy @(F rs))
      CSMS cs hs (MMM rs) = qprttableH o colnames (map stripNames (toList z))
   in tableString
        cs
        (oStyle o)
        (titlesH hs)
        (map (colsAllG top) rs)

-- there is no vertical vs horizontal! cos only one frame!

-- | print a frame of data
fprttableRec' ::
  forall rs t.
  (Foldable t, ReifyConstraint FromCell V.Identity rs, RFoldMap rs, HasCallStack) =>
  Opts ->
  t (Rec V.Identity rs) ->
  String
fprttableRec' o (toList -> z@(r1 : _)) =
  let len = getSum $ V.rfoldMap (const (Sum @Int 1)) r1
      colnames = map (\i -> '_' : show i) [1 .. len]
      CSMS cs hs (MMM rs) = qprttableH o colnames (toList z)
   in tableString
        cs
        (oStyle o)
        (titlesH hs)
        (map (colsAllG top) rs)
fprttableRec' _ _ = "empty table"

-- | printer for resultsets
class ZPrint a where
  -- | displays the results vertically (normal usecase)
  zprintV ::
    HasCallStack =>
    a ->
    Opts ->
    FnRSHeader ->
    Int ->
    (Int, String)

instance
  forall a.
  FromCell a =>
  ZPrint (RState (SelCol a))
  where
  zprintV z@RState{rsOut = SelCol _ meta} o fn pos =
    ( pos + 1
    , fn ("SelCol", pos) <> dumpMeta o meta <> prttableVCol o meta (rsOutUnwrap z) <> "\n"
    )

instance
  forall a.
  FromCell a =>
  ZPrint (RState (SelRowCol a))
  where
  zprintV z@RState{rsOut = SelRowCol _ meta} o fn pos =
    ( pos + 1
    , fn ("SelRowCol", pos) <> dumpMeta o meta <> prttableVCol o meta [rsOutUnwrap z] <> "\n"
    )

instance
  (KnownNat (TP.LengthT rs), ReifyConstraint FromCell V.Identity rs, RFoldMap rs) =>
  ZPrint (RState (SelRow (Rec V.Identity rs)))
  where
  zprintV z@RState{rsOut = SelRow _ meta} o fn pos =
    -- can use 'a' instead of Proxy @(F rs) but for some reason doesnt work for SelRow?
    (pos + 1, fn ("SelRow", pos) <> dumpMeta o meta <> qprttableV o (addColumnNameUsingMeta (recLen @rs) meta) [rsOutUnwrap z])

instance
  {-# OVERLAPPING #-}
  (F.ColumnHeaders rs, ReifyConstraint FromCell V.Identity (Unlabeled rs), StripFieldNames rs, RFoldMap (Unlabeled rs)) =>
  ZPrint (RState (SelRow (F rs)))
  where
  zprintV z@RState{rsOut = SelRow _ meta} o fn pos =
    (pos + 1, fn ("SelRow", pos) <> dumpMeta o meta <> qprttableV o (addMetaToColName (F.columnHeaders (Proxy @(F rs))) meta) [stripNames (rsOutUnwrap z)])

instance
  {-# OVERLAPPING #-}
  forall a xs.
  ( GS.HasDatatypeInfo a
  , GS.Generic a
  , GS.Code a ~ '[xs]
  , GS.All FromCell xs
  ) =>
  ZPrint (RState (SelRow a))
  where
  zprintV z@RState{rsOut = SelRow _ meta} o fn pos =
    ( pos + 1
    , fn ("SelRow", pos) <> dumpMeta o meta <> prttableV o meta [rsOutUnwrap z] <> "\n"
    )

instance
  (KnownNat (TP.LengthT rs), ReifyConstraint FromCell V.Identity rs, RFoldMap rs) =>
  ZPrint (RState (Sel (Rec V.Identity rs)))
  where
  zprintV z@RState{rsOut = Sel _ meta} o fn pos =
    -- can use 'a' instead of Proxy @(F rs) but for some reason doesnt work for SelRow?
    (pos + 1, fn ("Sel", pos) <> dumpMeta o meta <> qprttableV o (addColumnNameUsingMeta (recLen @rs) meta) (rsOutUnwrap z))

instance
  {-# OVERLAPPING #-}
  (F.ColumnHeaders rs, ReifyConstraint FromCell V.Identity (Unlabeled rs), StripFieldNames rs, RFoldMap (Unlabeled rs)) =>
  ZPrint (RState (Sel (F rs)))
  where
  zprintV z@RState{rsOut = Sel _ meta} o fn pos =
    -- can use 'a' instead of Proxy @(F rs) but for some reason doesnt work for SelRow?
    (pos + 1, fn ("Sel", pos) <> dumpMeta o meta <> qprttableV o (addMetaToColName (F.columnHeaders (rsOutUnwrap z)) meta) (map stripNames (rsOutUnwrap z)))

instance
  {-# OVERLAPPING #-}
  forall a xs.
  ( GS.HasDatatypeInfo a
  , GS.Generic a
  , GS.Code a ~ '[xs]
  , GS.All FromCell xs
  ) =>
  ZPrint (RState (Sel a))
  where
  zprintV z@RState{rsOut = Sel _ meta} o fn pos =
    (pos + 1, fn ("Sel", pos) <> dumpMeta o meta <> prttableV o meta (rsOutUnwrap z))

instance ZPrint (RState SelRaw) where
  zprintV z@RState{rsOut = SelRaw _ meta} o fn pos =
    ( pos + 1
    , fn ("SelRaw", pos)
        <> dumpMeta o meta
        <> concat (prtHRetCol o [Right @Int (meta, rsOutUnwrap z)])
    )

instance ZPrint (RState Upd) where
  zprintV z@RState{} _ fn pos = (pos + 1, fn ("Upd", pos) <> "Upd " <> show (rsOutUnwrap z))

instance ExprC op => ZPrint (RState (UpdN op)) where
  zprintV z@RState{} _ fn pos = (pos + 1, fn (opPretty True (evalC @op (Just (rsOutUnwrap z))), pos) <> "UpdN " <> show (rsOutUnwrap z))

instance (ZPrint (RState a), ZPrint (RState b)) => ZPrint (RState (a :+: b)) where
  zprintV r o fn pos =
    case (rsIn r, rsOut r) of
      (x :+: _, Or (Left w)) -> zprintV (RState x w) o (prefixMessage fn "Left ") pos
      (_ :+: y, Or (Right w)) -> zprintV (RState y w) o (prefixMessage fn "Right ") pos

instance ZPrint (RState a) => ZPrint (RState (May a)) where
  zprintV r o fn pos =
    case (rsIn r, rsOut r) of
      (MayP x, May (Just w)) -> zprintV (RState x w) o (prefixMessage fn "Left ") pos
      (MayP{}, May Nothing) -> (pos + 1, fn ("May", pos) <> " Nothing")

-- need to prefix Alle info for each zprint
instance ZPrint (RState a) => ZPrint (RState (Alle a)) where
  zprintV (RState (AlleP x) (Alle xs)) o fn pos =
    (pos + length xs,) $
      L.intercalate "\n" $
        map snd $
          zipWith
            (\a i -> zprintV (RState x a) o (prefixMessage fn "Alle ") i)
            xs
            [pos ..]

instance ZPrint (RState a) => ZPrint (RState (Some a)) where
  zprintV (RState (SomeP x) (Some xs)) o fn pos =
    (pos + length xs,) $
      L.intercalate "\n" $
        map snd $
          zipWith
            (\a i -> zprintV (RState x a) o (prefixMessage fn "Some ") i)
            (N.toList xs)
            [pos ..]

instance (ZPrint (RState a), ZPrint (RState b)) => ZPrint (RState (a :*: b)) where
  zprintV (RState (x :*: y) (Both lhs rhs)) o fn pos =
    ( pos + 2
    , snd (zprintV (RState x lhs) o (prefixMessage fn "Both(lhs) ") pos)
        <> snd (zprintV (RState y rhs) o (prefixMessage fn "Both(rhs) ") (pos + 1))
    )

instance ZPrint (RState a) => ZPrint (RState (Rev a)) where
  zprintV (RState (RevP x) (Rev xs)) o fn pos =
    zprintV (RState x xs) o (prefixMessage fn "Rev ") pos

instance (KnownNat m, KnownNat n, ZPrint (RState a)) => ZPrint (RState (Range m n a)) where
  zprintV (RState (RangeP x) (Range xs)) o fn pos =
    (pos + length xs,) $
      L.intercalate "\n" $
        map snd $
          zipWith
            ( \a i ->
                zprintV
                  (RState x a)
                  o
                  (prefixMessage fn ("Range " <> show (TP.pnat @m) <> "-" <> show (TP.pnat @n) <> " "))
                  i
            )
            xs
            [pos ..]

instance (KnownNat n, ZPrint (RState a)) => ZPrint (RState (Exact n a)) where
  zprintV (RState (ExactP x) (Exact xs)) o fn pos =
    (pos + length xs,) $
      L.intercalate "\n" $
        map snd $
          zipWith
            ( \a i ->
                zprintV
                  (RState x a)
                  o
                  (prefixMessage fn ("Exact " <> show (TP.pnat @n) <> " "))
                  i
            )
            (N.toList xs)
            [pos ..]

-- | displays sql output to the screen
prt :: V.RecAll RState rs ZPrint => Rec RState rs -> IO ()
prt = putStrLn . prttableRecV (poAscii defT)

-- | displays sql output to the screen at verbose setting 1
prt1 :: V.RecAll RState rs ZPrint => Rec RState rs -> IO ()
prt1 = putStrLn . prttableRecV (poAscii $ poVerbose 1 defT)

-- | displays sql output to the screen at verbose setting 2
prt2 :: V.RecAll RState rs ZPrint => Rec RState rs -> IO ()
prt2 = putStrLn . prttableRecV (poAscii $ poVerbose 2 defT)

-- | displays sql output to the screen at verbose setting 3
prt3 :: V.RecAll RState rs ZPrint => Rec RState rs -> IO ()
prt3 = putStrLn . prttableRecV (poAscii $ poVerbose 3 defT)

-- | 'Printer' directs the to the implementation that handles printing
class Printer a where
  wfn :: HasCallStack => Opts -> a -> [String]

instance Printer [ResultSet] where
  wfn = prtHRetCol

instance
  (Foldable t, ReifyConstraint FromCell V.Identity rs, RFoldMap rs) =>
  Printer (t (Rec V.Identity rs))
  where
  wfn o a = [fprttableRec' o a]

instance
  (Foldable t, RFoldMap rs, ReifyConstraint FromCell V.Identity rs, RMap rs) =>
  Printer (t (Rec L.Identity rs))
  where
  wfn o a = [fprttableRec' o (map (V.rmap (V.Identity . L.runIdentity)) (toList a))]

-- used in frames!! eg TestTableFrames
instance
  (Foldable t, StripFieldNames rs, F.ColumnHeaders rs, ReifyConstraint FromCell V.Identity (Unlabeled rs), RFoldMap (Unlabeled rs)) =>
  Printer (t (F rs))
  where
  wfn o a = [fprttableRecT o a]

instance V.RecAll RState rs ZPrint => Printer (Rec RState rs) where
  wfn o a = [prttableRecV o a]

-- | pretty print sql output as a string
wstring :: Printer a => a -> String
wstring = wstringWith defT

-- | pretty print sql output as a string in verbose mode
wstring' :: Printer a => Int -> a -> String
wstring' i = wstringWith (poAscii $ poVerbose i defT)

-- | pretty print sql output as a string using options
wstringWith :: Printer a => Opts -> a -> String
wstringWith o = L.intercalate "\n" . wfn o

-- | 'wprint' displays sql output to the screen using default settings 'defT'
wprint :: Printer a => a -> IO ()
wprint = wprintWith defT

-- | 'wprint' displays sql output to the screen using ascii
wprint' :: Printer a => Int -> a -> IO ()
wprint' i = wprintWith (poAscii $ poVerbose i defT)

-- | displays sql output to the screen using passed options
wprintWith' :: Printer a => Opts -> a -> IO ()
wprintWith' o = wprintWith (poAscii o)

-- | displays sql output to the file
wprintfn :: Printer a => FilePath -> a -> IO ()
wprintfn fn = wprintWith (poFile fn defT)

-- | displays sql output to the file with verbosee option
wprintfn' :: Printer a => Int -> FilePath -> a -> IO ()
wprintfn' i fn = wprintWith (poFile fn (poAscii $ poVerbose i defT))

-- | displays sql output to the screen restricting the columns displayed
wprintCols :: Printer a => ([Int] -> [Int]) -> a -> IO ()
wprintCols fn = wprintWith (poCols fn defT)

-- | displays sql output to the screen restricting the columns displayed in ascii mode
wprintCols' :: Printer a => ([Int] -> [Int]) -> a -> IO ()
wprintCols' fn = wprintWith (poCols fn (poAscii defT))

-- | displays sql output using the options given
wprintWith :: Printer a => Opts -> a -> IO ()
wprintWith o xs = case oFile o of
  Nothing -> putStrLn $ L.intercalate "\n" $ wfn o xs
  Just fn -> SIO.withFile fn SIO.AppendMode $ \h -> do
    SIO.hSetEncoding h SIO.utf8
    SIO.hPutStrLn h $ L.intercalate "\n" $ wfn o xs
