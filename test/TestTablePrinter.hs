{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module TestTablePrinter where

import Control.Lens
import Control.Monad.State.Strict
import Data.Char
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as N
import Data.Semigroup
import Data.Semigroup.Foldable (intercalateMap1)
import qualified Data.Text as T
import Data.Time
import Data.Vinyl
import Data.Vinyl.Syntax ()
import Database.HDBC.ColTypes
import qualified GHC.Generics as G
import qualified Generics.SOP as GS
import HSql.Core
import HSql.Core.RStateBuilder
import HSql.Core.SqlBuilder
import HSql.Core.TablePrinter
import Test.Hspec
import TestSqlHelper
import Text.Layout.Table
import Text.Shakespeare.Text

spec :: SpecWith ()
spec =
  describe "Type Tests" $
    it "should allow combined Exact and Alle with no Alle data" $
      True `shouldBe` True

testupd1 :: Rec RState '[Sel (Bool, Char)]
testupd1 = mkRState (mkSel ["field1", "field2"] [(False, 'x'), (True, 'y')]) :& RNil

testselrawSynth :: Rec RState '[SelRaw]
testselrawSynth =
  mkRState
    ( mkSelRaw
        ["field1", "field2", "field3", "field4"]
        [ [SqlString "asdf", SqlZonedLocalTimeOfDay (TimeOfDay 1 2 3) (TimeZone (-360) True "mytimezone"), SqlInt32 123]
        , [SqlNull, SqlZonedLocalTimeOfDay (TimeOfDay 12 22 59) utc, SqlInt32 9999]
        ]
    )
    :& RNil

testselraw0 :: Rec RState '[SelRaw]
testselraw0 = mkRState (mkSelRaw ["field1", "field2", "field3", "field4"] [[SqlString "asdf", SqlNull, SqlInt32 123], [SqlNull, SqlBool True, SqlInt32 9999]]) :& RNil

testselraw1 :: Rec RState '[SelRaw]
testselraw1 = mkRState (mkSelRaw [] [[SqlString "asdf", SqlNull, SqlInt32 123], [SqlNull, SqlBool True, SqlInt32 9999]]) :& RNil

testselraw2 :: Rec RState '[SelRaw]
testselraw2 = mkRState (mkSelRaw [] [[SqlString "asdf"]]) :& RNil

testselraw3 :: Rec RState '[SelRaw]
testselraw3 = mkRState (mkSelRaw [] [[SqlString "col1a", SqlString "col2a", SqlString "col3a"], [SqlString "col1b", SqlString "col2b", SqlString "col3b"]]) :& RNil

testselraw4 :: Rec RState '[SelRaw]
testselraw4 = mkRState (mkSelRaw [] (map (map SqlInt32) [[99, 9999999], [1234, 2], [12, 2], [1, 9999]])) :& RNil

testselraw5 :: Rec RState '[SelRaw]
testselraw5 = mkRState (mkSelRaw [] (map (map SqlDouble) [[99, 9999999], [12.34, 2], [12, 2], [1, 9999.1234445]])) :& RNil

testselrawfail :: Rec RState '[SelCol [SqlValue]]
testselrawfail = mkRState (mkSelCol [] [[SqlString "asdf"]]) :& RNil

testselraw6 :: Rec RState '[SelRaw]
testselraw6 = mkRState (mkSelRaw [] [[SqlString "asdf", SqlNull, SqlInt32 123], [SqlNull, SqlBool True, SqlInt32 9999]]) :& RNil

testp00 :: Rec RState '[SelRow (Maybe Int)]
testp00 = mkRState (mkSelRow [] (Just 4)) :& RNil

testTuples :: Rec RState '[Sel (Int, (String, Bool, Double), (Double, Int))]
testTuples = mkRState (mkSel ["field1", "field2", "field3", "field4", "field5", "field6"] [(123, ("aa", True, 3.4), (1.1, 33))]) :& RNil

testp01 :: Rec RState '[SelRow (One (Maybe Int))]
testp01 = mkRState (mkSelRow [] (One (Just 4))) :& RNil

testNonEmpty :: Rec RState '[SelRowCol (DecNE 5 Int)]
testNonEmpty = mkRState (mkSelRowCol (map (\i -> "field" <> show @Int i) [1 .. 5]) (DecNE (101 :| [102 .. 105]))) :& RNil

-- the unOne should progress through each field
testNonEmptyOne :: Rec RState '[SelRow (One (DecNE 5 Int))]
testNonEmptyOne = mkRState (mkSelRow (map (\i -> "field" <> show @Int i) [1 .. 5]) (One (DecNE (101 :| [102 .. 105])))) :& RNil

testp0 :: Rec RState '[Sel Int]
testp0 = mkRState (mkSel [] [4, 5, 6]) :& RNil

testp1 :: Rec RState '[SelRow Bool, SelRow Char]
testp1 = mkRState (mkSelRow [] True) :& mkRState (mkSelRow [] 'c') :& RNil

testp1a :: Rec RState '[SelRow (Int, String), SelRow (Int, String)]
testp1a =
  mkRState (mkSelRow [] (1, "hello\nworld and more data"))
    :& mkRState (mkSelRow [] (2, "this is a test afield\r\n\tx\n\n\nxx"))
    :& RNil

testp1b :: Rec RState '[Sel (Int, String), Sel (Int, String)]
testp1b =
  mkRState (mkSel [] [(1, "hello\nworld and more data"), (2, "some text")])
    :& mkRState (mkSel [] [(99, "this is a test afield\r\n\tx\n\n\nxx"), (100, "some stuff")])
    :& RNil

testp1c :: Rec RState '[Sel (String, Int), Sel (String, Int)]
testp1c =
  mkRState (mkSel [] [("hello\nworld and more data", 1), ("some text", 2)])
    :& mkRState (mkSel [] [("this is a test afield\r\n\tx\n\n\nxx", 99), ("some stuff", 100)])
    :& RNil

testp2 :: Rec RState '[Sel (One Bool), Sel (One Char)]
testp2 =
  mkRState (mkSel [] (map One [False, True]))
    :& mkRState (mkSel [] (map One ['c', 'd']))
    :& RNil

testp3 :: Rec RState '[Sel (Bool, Char)]
testp3 = mkRState (mkSel [] [(False, 'x'), (True, 'y')]) :& RNil

testp4 :: Rec RState '[Sel (Bool, Char), Sel (Int, Bool, Char)]
testp4 =
  mkRState (mkSel [] [(False, 'x'), (True, 'y')])
    :& mkRState (mkSel [] [(1, False, 'x'), (3, True, 'y')])
    :& RNil

testp5 :: Rec RState '[SelRow (Bool, Char), SelRow (Int, Bool, Char)]
testp5 =
  mkRState (mkSelRow [] (False, 'x'))
    :& mkRState (mkSelRow [] (1, True, 'y'))
    :& RNil

testp6 :: Rec RState '[SelRow (Bool, Char), SelRow (String, Int), Sel (Double, Int, String)]
testp6 =
  mkRState (mkSelRow [] (False, 'x'))
    :& mkRState (mkSelRow [] ("hey man", 123))
    :& mkRState (mkSel [] [(1.3, 222, "aaa"), (1.4, 333, "bbb"), (1.5, 444, "ccc")])
    :& RNil

testp7 :: Rec RState '[Sel (Int, Char), SelRow (String, Int), Sel (Double, Int, String)]
testp7 =
  mkRState (mkSel [] [(10, 'x'), (11, 'y'), (12, 'z'), (13, 'w')])
    :& mkRState (mkSelRow [] ("hey man", 123))
    :& mkRState (mkSel [] [(1.3, 222, "aaa"), (1.4, 333, "bbb"), (1.5, 444, "ccc"), (1.6, 555, "ddd"), (1.7, 666, "eee"), (1.8, 777, "fff")])
    :& RNil

testp8 :: Rec RState '[Sel (Int, Char), SelRow (String, Int), Upd, Sel (Double, Int, String)]
testp8 =
  mkRState (mkSel [] [(10, 'x'), (11, 'y'), (12, 'z'), (13, 'w')])
    :& mkRState (mkSelRow [] ("hey man", 123))
    :& mkRState (mkUpd 4949)
    :& mkRState (mkSel [] [(1.3, 222, "aaa"), (1.4, 333, "bbb"), (1.5, 444, "ccc"), (1.6, 555, "ddd"), (1.7, 666, "eee"), (1.8, 777, "fff")])
    :& RNil

testp9a, testp9b :: Rec RState '[Sel (Bool, Char) :+: Sel (Int, Bool, Char)]
testp9a = mkRState (mkSel [] [(False, 'x'), (True, 'y')] |<| mkSel [] [(1 :: Int, False, 'x'), (3, True, 'y')]) :& RNil
testp9b = mkRState (mkSel [] [(False, 'x'), (True, 'y')] |>| mkSel [] [(1 :: Int, False, 'x'), (3, True, 'y')]) :& RNil

testp10 :: Rec RState '[Alle (Sel (Bool, Char))]
testp10 =
  mkRState (mkAlle (replicate 4 (mkSel [] [(False, 'x'), (True, 'y')])))
    :& RNil

testp11 :: Rec RState '[Exact 3 (Sel (Bool, Char))]
testp11 =
  let z = mkSel [] [(False, 'x'), (True, 'y')]
   in mkRState (mkExact (z :| replicate 4 z)) :& RNil

testp11a :: Rec RState '[Rev (Sel (Bool, Char)), SelRow (Char, Bool)]
testp11a =
  mkRState (mkRev (mkSel [] [(False, 'x'), (True, 'y')]))
    :& mkRState (mkSelRow [] ('x', False))
    :& RNil

-- this works even tho a single value but it is wrapped in T2
testp12 :: Rec RState '[Sel T2]
testp12 = mkRState (mkSel [] [T2 11, T2 123]) :& RNil

-- | test multiple values using generics-sop
testp12a :: Rec RState '[Sel T4]
testp12a = mkRState (mkSel [] [T4 'x' 11, T4 'y' 123]) :& RNil

testp12b :: Rec RState '[Sel (Char, Double)]
testp12b = mkRState (mkSel [] [('x', 11), ('y', 123)]) :& RNil

-- singles dont print without One
testp13 :: Rec RState '[Sel (One Int)]
testp13 = mkRState (mkSel [] [One 11, One 123, One 1233]) :& RNil

testp13a :: DefDec (Dec a) => [a] -> Rec RState '[Sel (One a)]
testp13a xs = mkRState (mkSel [] (map One xs)) :& RNil

testp14 :: Rec RState '[SelRaw]
testp14 = mkRState (mkSelRaw [] [[SqlInt32 12, SqlBool False], [SqlInt32 12, SqlBool False]]) :& RNil

newtype T2 = T2 {t22 :: Int} deriving stock (G.Generic, Show)
instance GS.Generic T2
instance GS.HasDatatypeInfo T2
instance DefDec (Dec T2) where
  defDec = T2 <$> defDec

newtype T3 = T3 {t33 :: Bool} deriving stock (G.Generic, Show)
instance GS.Generic T3
instance GS.HasDatatypeInfo T3
instance DefDec (Dec T3) where
  defDec = T3 <$> defDec

data T4 = T4 {t44 :: !Char, t444 :: !Double} deriving stock (G.Generic, Show)
instance GS.Generic T4
instance GS.HasDatatypeInfo T4
instance DefDec (Dec T4) where
  defDec = T4 <$> defDec <*> defDec

data T1 = T1 {ta1 :: !String, ta2 :: !Bool, ta3 :: !String, ta4 :: !Int, ta5 :: !Char, ta6 :: !(Maybe Double)} deriving stock (G.Generic, Show)
instance GS.Generic T1
instance GS.HasDatatypeInfo T1

t11, t12, t13 :: T1
t11 = T1 "hello" True "world" 123 'x' (Just 123.4556)
t12 = T1 "d" False "hello there \nworld" (-123) 'x' (Just 123.4556)
t13 = T1 "hello" True s2a 99 'x' Nothing

s1, s2, s1a, s2a, s1b, s2b :: String
s1 =
  T.unpack
    [st|this is a long rant
of data with some interesting
stuff ansd this is the end of it|]
s2 =
  T.unpack
    [st|this is more data and here we go 1233444544
xxxxx                       yyyy
dide asef sadf sdf sdaf sdf|]
s1a = T.unpack [st|this is a long rant of data with some interesting stuff ansd this is the end of it|]
s2a = T.unpack [st|this is more data and here we go 1233444544 dide asef sadf sdf sdaf sdf|]
s1b = T.unpack [st|this is a long rant of data with some interesting stuff ansd this is the end of it|]
s2b = T.unpack [st|znork|]

t1s :: [Char -> Maybe Double -> T1]
t1s = [T1 s1 True s2 4444, T1 s1a False s2a 123, T1 s1b True s2b (-123)]

z1, z2, z3, z4 :: IO ()
z1 =
  putStrLn $
    tableString
      [def, numCol]
      unicodeS
      def
      [ rowG ["Jack" :: String, "184.74"]
      , rowG ["Jane" :: String, "162.2"]
      ]
z2 = do
  let xx = top -- center -- vs top bottom
  putStrLn $
    tableString
      [fixedLeftCol 50, def, fixedLeftCol 30, numCol]
      asciiS
      (titlesH ["title1", "Bool", "title2", "SomeNum"])
      [ colsAllG xx [justifyText 50 s1, ["True"], justifyText 50 ("afield\t\t" ++ s2 ++ "\t\txx"), ["4444.1"]]
      , colsAllG xx [justifyText 50 s1a, ["False"], justifyText 50 s2a, ["1.2"]]
      , colsAllG xx [justifyText 50 s1b, ["True"], justifyText 50 s2b, ["2222229"]]
      ]

-- have to get rid of newlines in the text hence using justifyText to do that
z3 =
  putStrLn $
    tableString
      [fixedLeftCol 65, def, fixedLeftCol 30, numCol]
      unicodeS
      def
      [ rowG [concat (justifyText 200 s1), "True", s2, "4444.1"]
      , rowG [s1a, "False", s2a, "299991.2"]
      , rowG [s1b, "True", s2b, "1.2"]
      ]

txt1 :: String
txt1 =
  T.unpack
    [st|Lorem ipsum  dolor sit amet, consectetur adipisici
elit,  sed eiusmod  tempor incidunt  ut labore  et
dolore magna aliqua. Ut enim ad minim veniam, quis
nostrud   exercitation  ullamco  laboris  nisi  ut
aliquid  ex ea  commodi consequat.  Quis aute iure
reprehenderit   in  voluptate  velit  esse  cillum
dolore  eu fugiat  nulla pariatur.  Excepteur sint
obcaecat cupiditat non proident, sunt in culpa qui
officia deserunt mollit anim id est laborum.|]

z4 =
  putStrLn $
    tableString
      [fixedLeftCol 50, numCol, numCol]
      asciiS
      (titlesH ["Text", "Length", "afield"])
      [ colsAllG
          center
          [ justifyText 50 txt1
          , [show $ length txt1]
          , ["12345"]
          ]
      ]

cols1 :: [ResultSet]
cols1 =
  [ Right
      (
      [ (SqlColDesc{colName = "SomeId", colType = SqlVarCharT, colSize = Just 20, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False})
      , (SqlColDesc{colName = "SerialNum", colType = SqlIntegerT, colSize = Just 10, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False})
      , (SqlColDesc{colName = "Guid", colType = SqlVarCharT, colSize = Just 100, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False})
      , (SqlColDesc{colName = "DateAdded", colType = SqlTimestampT, colSize = Just 23, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False})
      , (SqlColDesc{colName = "SomeId2", colType = SqlVarCharT, colSize = Just 20, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just True})
      ]
      ,
      [ [SqlByteString "1234\n\r\n\n5678", SqlInt32 12345, SqlByteString "888A8AA8-AAAA-BBBB-CCCC--A1111AA1A11A", SqlLocalTime (read "2016-04-18 14:33:12.54"), SqlByteString "012345678"]
      , [SqlByteString "23456788", SqlInt32 12346, SqlByteString "888A8AA8-AAAA-BBBB-CCCC--A1111AA1A11A", SqlLocalTime (read "2016-04-18 14:33:12.54"), SqlByteString "023456788"]
      , [SqlByteString "28763335", SqlInt32 12347, SqlByteString "888A8AA8-AAAA-BBBB-CCCC--A1111AA1A11A", SqlLocalTime (read "2016-04-18 14:33:12.54"), SqlByteString "028763335"]
      , [SqlByteString "34567889", SqlInt32 12348, SqlByteString "888A8AA8-AAAA-BBBB-CCCC--A1111AA1A11A", SqlLocalTime (read "2016-04-18 14:33:12.54"), SqlByteString "034567889"]
      , [SqlByteString "45678890", SqlInt32 12349, SqlByteString "888A8AA8-AAAA-BBBB-CCCC--A1111AA1A11A", SqlLocalTime (read "2016-04-18 14:33:12.54"), SqlByteString "045678890"]
      , [SqlByteString "567889012", SqlInt32 23491, SqlByteString "888A8AA8-AAAA-BBBB-CCCC--A1111AA1A11A", SqlLocalTime (read "2016-04-18 14:33:12.54"), SqlByteString "567889012"]
      , [SqlByteString "67889012", SqlInt32 23492, SqlByteString "888A8AA8-AAAA-BBBB-CCCC--A1111AA1A11A", SqlLocalTime (read "2016-04-18 14:33:12.54"), SqlByteString "067889012"]
      , [SqlByteString "78890123", SqlInt32 23493, SqlByteString "888A8AA8-AAAA-BBBB-CCCC--A1111AA1A11A", SqlLocalTime (read "2016-04-18 14:33:12.54"), SqlByteString "078890123"]
      , [SqlByteString "88901234", SqlInt32 23494, SqlByteString "888A8AA8-AAAA-BBBB-CCCC--A1111AA1A11A", SqlLocalTime (read "2016-04-18 14:33:12.54"), SqlByteString "088901234"]
      , [SqlByteString "890123456", SqlInt32 23495, SqlByteString "888A8AA8-AAAA-BBBB-CCCC--A1111AA1A11A", SqlLocalTime (read "2016-04-18 14:33:12.54"), SqlByteString "890123456"]
      ]
      )
  ]

newtype F1 = F1 {unF1 :: String} deriving stock (Show, Eq, G.Generic)

instance GS.Generic F1
instance GS.HasDatatypeInfo F1

instance DefDec (Dec F1) where
  defDec = F1 <$> defDec

testF1Col :: Rec RState '[SelCol F1]
testF1Col = mkRState (mkSelCol ["Xf1"] [F1 "hello world", F1 "that is all"]) :& RNil

data F3 = F3 {f3A :: String, f3B :: Int, f3C :: String} deriving stock (Show, Eq, G.Generic)

instance GS.Generic F3
instance GS.HasDatatypeInfo F3

instance FromCell F3 where
  fromCell o iss lr (F3 x y z) = do
    let iss' = iss |> (1, "F3")
    sconcat <$> sequence (fromCell o iss' lr x :| [fromCell o iss' lr y, fromCell o iss' lr z])

instance DefDec (Dec F3) where
  defDec = F3 <$> defDec <*> defDec <*> defDec

-- testF3A works but this doesnt print the extra meta columns unless you wrap in SelCol or One
testF3One :: Rec RState '[Sel (One F3)]
testF3One = mkRState (mkSelOne ["Xf3a", "Xf3b", "Xf3c"] [F3 "hello world" 13 "abcd", F3 "that is all" 14 "efgh"]) :& RNil

testF3Col :: Rec RState '[SelCol F3]
testF3Col = mkRState (mkSelCol ["Xf3a", "Xf3b", "Xf3c"] [F3 "hello world" 13 "abcd", F3 "that is all" 14 "efgh"]) :& RNil

testF3 :: Rec RState '[Sel F3]
testF3 = mkRState (mkSel ["Xf3a", "Xf3b", "Xf3c"] [F3 "hello world" 13 "abcd", F3 "that is all" 14 "efgh"]) :& RNil

-- adds the extra metacolumns when using wprint for F3
testF3A :: Rec RState '[Sel (String, F3)]
testF3A = mkRState (mkSel ["field1", "Xf3a", "Xf3b", "Xf3c"] [("xyz" :: String, F3 "hello world" 13 "abcd"), ("abc", F3 "that is all" 14 "efgh")]) :& RNil

cols2 :: [ResultSet]
cols2 = [Left 123, Left 456]

testIP4 :: Rec RState '[SelCol IP4]
testIP4 = mkRState (mkSelCol ["#ip1", "#ip2", "#ip3", "#ip4"] [IP4 1 2 3 4, IP4 141 214 125 56]) :& RNil

testIP4One :: Rec RState '[Sel (One IP4)]
testIP4One = mapRState1 (\(SelCol xs m) -> Sel (map One xs) m) testIP4

testIP4Pair :: Rec RState '[Sel (IP4, IP4)]
testIP4Pair = mapRState1 (\(SelCol xs m) -> Sel (zip xs xs) (m <> m)) testIP4

data IP4 = IP4 {ip1 :: !Int, ip2 :: !Int, ip3 :: !Int, ip4 :: !Int} deriving stock (G.Generic, Show, Eq)

instance GS.Generic IP4
instance GS.HasDatatypeInfo IP4

instance DefDec (Dec IP4) where
  defDec = IP4 <$> defDec <*> defDec <*> defDec <*> defDec

instance FromCell IP4 where
  fromCell o iss lr (IP4 a b c d) = do
    cstate <- get
    let hdrx = askHeaderS o lr cstate
    -- RealHeader th -> case th of

    cs <- traverse (fromCell o (iss |> (2, "[" <> show hdrx <> "]") |> (1, "IP4")) lr) (a :| [b, c, d])

    -- todo
    --    let rs = L.intercalate "_" $ map (getPrefixFromHeaderS o . cellHeader . N.head) (N.toList cs)
    return $
      pure
        Cell
          { cellValue = intercalateMap1 "." (intercalateMap1 "|" cellValue) cs
          , --  { cellValue = L.intercalate "." (map (L.intercalate "|" . map cellValue . N.toList) (N.toList cs))
            cellColSpec = upto (oRC o ^. _2)
          , cellFieldType = Stringy
          , cellHeaderPrefix = iss |> (1, "IP4Extra") -- todo
          --  , cellHeader = SyntheticHeader $ L.intercalate "_" $ N.toList $ N.map (showHeaderS o) $ sconcat cs
          , cellHeader = SyntheticHeader $ intercalateMap1 "_" (showHeaderS o) $ sconcat cs
          }

data IP a = IP {_octet1 :: !a, _octet2 :: !a, _octet3 :: !a, _octet4 :: !a} deriving stock (Show, Eq)

testCompiles1 :: IO ()
testCompiles1 = putStrLn $ prttableRecV defT (RState (SelRowP @(One Int) defDec) (SelRow (One 4) []) :& RNil)

testCompiles2 :: IO ()
testCompiles2 = putStrLn $ prttableRecV defT (RState (SelP @(One Int) defDec) (Sel [One 4, One 5, One 6] [metaDefString 30 "x"]) :& RNil)

testCompiles3 :: IO ()
testCompiles3 = putStrLn $ prttableRecV defT (RState (SelRowP @(Int, Int) defDec) (SelRow (4, 5) [metaDefString 30 "x", metaDefString 30 "y"]) :& RNil)

-- have to explicitly set the types eg Int/Integer (unlike in ghci)
testPrt1 :: IO ()
testPrt1 = prt $ mkRState (mkSelRowOne [] (12 :: Int)) :& mkRState (mkSelOne ["xx"] [1 :: Int .. 4]) :& RNil

testPrt2 :: IO ()
testPrt2 = prt $ mkRState (mkSelRowOne [] (12 :: Integer) `mkBoth` mkSelOne ["xx"] [1 :: Int .. 4]) :& RNil

testPrt3 :: IO ()
testPrt3 = prt $ mkRState (mkSel ["hhh", "kkkk"] [(True, 'x', "abc" :: String), (False, 'z', "def"), (True, 'w', "ghi")]) :& RNil

testPrt4 :: IO ()
testPrt4 = prt $ mkRState (mkSel ["hhh", "kkkk"] [(True, 'x', "abc" :: String), (False, 'z', "def"), (True, 'w', "ghi")]) :& RNil

testPrt5 :: IO ()
testPrt5 = prt $ mkRState (mkOr @_ @Upd (Left (mkSel [] [(True, 'x'), (False, 'w')]))) :& RNil

testPrt5A :: IO ()
testPrt5A = prt $ mkRState (mkOr @(Sel (Int, Bool)) @(UpdN ( 'OElem (PosList (4 ':| '[5, 9])))) (Right (mkUpdN 13))) :& RNil

testPrt5B :: IO ()
testPrt5B = prt $ mkRState (mkOr @(Sel (Int, Bool)) @(UpdN ( 'OElem (PosList (4 ':| '[5, 9])))) (Right (mkUpdN 5))) :& RNil

testPrt6 :: IO ()
testPrt6 =
  let ns = N.map (\i -> mkSel ["mycol1", "othercol2"] [(show i, chr (ord 'a' + i)), (show (i * 2), chr (ord 'a' + i * 2))]) ((1 :: Int) :| [2 .. 4])
   in prt $ mkRState (mkExact @4 ns) :& RNil

testPrt7 :: IO ()
testPrt7 = prt $ mkRState (mkSelRowCol ["fred"] 'x') :& RNil

testPrt8 :: IO ()
testPrt8 = prt $ mkRState (mkSelRowCol [] (True, False)) :& RNil

testPrt9 :: IO ()
testPrt9 = prt2 $ mkRState (mkSelCol ["fred"] ["a" :: String, "b", "c"]) :& RNil

testPrt10 :: IO ()
testPrt10 = prt $ processRetCol @'[SelRowCol Int] defDec [intRS1 3] ^?! _Right

testPrt11 :: IO ()
testPrt11 = prt $ rstateChange1 (mapRState (\(Sel xs m) -> SelCol xs m)) $ mkRState (mkSel ["hhh", "kkkk", "lll"] [(True, 'x', "abc" :: String), (False, 'z', "def"), (True, 'w', "ghi")]) :& RNil

testPrt12 :: IO ()
testPrt12 = prt $ rstateChange1 (mapRState (\(Sel xs m) -> SelCol (map (view _2) xs) [m !! 1])) $ mkRState (mkSel ["hhh", "kkkk", "lll"] [(True, 'x', "abc" :: String), (False, 'z', "def"), (True, 'w', "ghi")]) :& RNil

-- testPrt13 :: IO () -- this wont compile but if we convert to selToSelCol then it works
-- testPrt13 = prt $ mkRState (mkSel ["hhh"] ["abc" :: String, "def", "ghi"]) :& RNil

testPrt14 :: IO ()
testPrt14 = prt $ mapRState1 (\(Sel xs m) -> SelCol xs m) $ mkRState (mkSel ["hhh"] ["abc" :: String, "def", "ghi"]) :& RNil

-- or wrap with One
testPrt15 :: IO ()
testPrt15 = prt $ mapRState1 (\(Sel xs m) -> Sel (map One xs) m) $ mkRState (mkSel ["hhh"] ["abc" :: String, "def", "ghi"]) :& RNil

-- or wrap with One
testPrt16 :: IO ()
testPrt16 = prt $ mapRState1 (\(Sel xs m) -> Sel (map One xs) m) $ mkRState (mkSel ["hhh", "kkkk", "lll"] [(True, 'x', "abc" :: String), (False, 'z', "def"), (True, 'w', "ghi")]) :& RNil

data T12 = T12 {t12A :: !String, t12B :: !String} deriving stock (G.Generic, Show, Eq)

instance GS.Generic T12
instance GS.HasDatatypeInfo T12

instance DefDec (Dec T12) where
  defDec = T12 <$> defDec <*> defDec

instance FromCell T12 where
  fromCell o iss _lr (T12 a b) =
    sconcat
      <$> traverse
        (uncurry (fromCell o (iss |> (1, "T12"))))
        ( (RealField, a)
            :| [ (SyntheticField "t12A_syn", "t12A:..")
               , (RealField, b)
               , (SyntheticField "t12B_syn", "t12B:...")
               ]
        )

data T13 = T13 {t13A :: !String, t13B :: !String, t13C :: !String} deriving stock (G.Generic, Show, Eq)

instance GS.Generic T13
instance GS.HasDatatypeInfo T13

instance DefDec (Dec T13) where
  defDec = T13 <$> defDec <*> defDec <*> defDec

instance FromCell T13 where
  fromCell o iss _lr (T13 a b c) =
    sconcat
      <$> traverse
        (uncurry (fromCell o (iss |> (1, "T13"))))
        ( (RealField, a)
            :| [ (SyntheticField "t13A_syn", "t13A:...")
               , (RealField, b)
               , (SyntheticField "t13B_syn", "t13B:...")
               , (RealField, c)
               , (SyntheticField "t13C_syn", "t13C:...")
               ]
        )

unSel ::
  forall xs a.
  ( GS.HasDatatypeInfo a
  , GS.Code a ~ '[xs]
  , GS.All FromCell xs
  ) =>
  Opts ->
  Rec RState '[Sel a] ->
  IO ()
unSel o (r :& RNil) = do
  let Sel xs m = rsOut r
  putStrLn $ prttableV o m xs

testT12 :: Rec RState '[Sel T12]
testT12 = mkRState (mkSel ["t12a", "t12b"] [T12 "t12a:hello world" "t12b:another round", T12 "t12a:no worries" "t12b:she'll be right"]) :& RNil

testT12One :: Rec RState '[Sel (One T12)]
testT12One = mapRState1 (\(Sel xs m) -> Sel (map One xs) m) testT12

testT13 :: Rec RState '[Sel T13]
testT13 = mkRState (mkSel ["t13a", "t13b", "t13c"] [T13 "t13a:bruce" "t13b:sheila" "t13c:john", T13 "t13a:mikey" "t13b:graham" "t13c:peter"]) :& RNil

testT1213 :: Rec RState '[Sel (T12, T13)]
testT1213 =
  mkRState
    ( mkSel
        ["t12a", "t12b", "t13a", "t13b", "t13c"]
        [ (T12 "t12a:hello world" "t12b:another round", T13 "t13a:bruce" "t13b:sheila" "t13c:john")
        , (T12 "t12a:no worries" "t12b:she'll be right", T13 "t13a:mikey" "t13b:graham" "t13c:peter")
        ]
    )
    :& RNil

testT1213One :: Rec RState '[Sel (One (T12, T13))]
testT1213One = mapRState1 (\(Sel xs m) -> Sel (map One xs) m) testT1213

data T123Holder = T123Holder {t123A :: !T12, t123B :: !T13} deriving stock (G.Generic, Show, Eq)

instance GS.Generic T123Holder
instance GS.HasDatatypeInfo T123Holder

instance DefDec (Dec T123Holder) where
  defDec = T123Holder <$> defDec <*> defDec

instance FromCell T123Holder where
  fromCell o iss lr (T123Holder a b) = do
    x <- fromCell o (iss |> (1, "T123Holder")) lr a
    y <- fromCell o (iss |> (1, "T123Holder")) lr b
    return (x <> y)

testT1213Holder :: Rec RState '[Sel T123Holder]
testT1213Holder =
  mkRState
    ( mkSel
        ["t12a", "t12b", "t13a", "t13b", "t13c"]
        [ T123Holder (T12 "t12a:hello world" "t12b:another round") (T13 "t13a:bruce" "t13b:sheila" "t13c:john")
        , T123Holder (T12 "t12a:no worries" "t12b:she'll be right") (T13 "t13a:mikey" "t13b:graham" "t13c:peter")
        ]
    )
    :& RNil

testT1213OneHolder :: Rec RState '[Sel (One (T123Holder, Char))]
testT1213OneHolder = mapRState1 (\(Sel xs m) -> Sel (map (One . (,'x')) xs) (m <> [metaDefString 30 "singlechar"])) testT1213Holder
