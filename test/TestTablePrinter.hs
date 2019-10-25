{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS -Wall -Wcompat -Wincomplete-record-updates -Wincomplete-uni-patterns -Wredundant-constraints #-}
module TestTablePrinter where
import Data.Data
import Text.Shakespeare.Text
import qualified Data.Text as T
import Data.Text (Text)
import Text.Layout.Table
import qualified GHC.Generics as G
import qualified Generics.SOP as GS
import Control.Lens hiding (from, to)
import Data.Vinyl
import Data.Vinyl.Syntax ()  -- orphan instance so we can use #abc as a lens!
import Sql
import Data.Bool
import TablePrinter
import Database.HDBC.ColTypes
import Test.Hspec
import qualified Frames as F
import Frames (Frame(..))
import Frames.CSV -- (readTableOpt, rowGen, RowGen(..))
import qualified Pipes as P -- hiding (Proxy)

-- use csv reader from Frames and then manipulate that!
F.tableTypes "User" "MOCK_DATA_SMALL.csv"

$(F.declareColumn "fred1" ''Int)
-- type Fred1 = "fred1" :-> Int :: (Symbol, *)
$(F.declareColumn "Fred2" ''Int)  -- this works but cos uppercase first letter we cant use # label
-- type Fred2 = "Fred2" :-> Int :: (Symbol, *)

userStream :: F.MonadSafe m => P.Producer User m ()
userStream = readTableOpt userParser "MOCK_DATA_SMALL.csv"

loadUsers :: IO (Frame User)
loadUsers = F.inCoreAoS userStream


testCsvPrint :: IO ()
testCsvPrint = loadUsers >>= wprint

spec :: SpecWith ()
spec =
  describe "Type Tests" $
    it "should allow combined Some and Alle with no Alle data" $
      True `shouldBe` True

testupd1 :: Rec ZZZ '[Sel (Bool, Char)]
testupd1 = let xs = [(False,'x'), (True,'y')] in ZZZ (SelP defDec) (Sel xs) xs [] :& RNil

testeither :: Bool -> ZZZ a -> ZZZ b -> ZZZ (a :+: b)
testeither p (ZZZ a1 a2 a3 a4) (ZZZ b1 b2 b3 b4) = ZZZ (a1 :+: b1) (EitherRS (bool (Left a2) (Right b2) p)) (bool (Left a3) (Right b3) p) (bool a4 b4 p)

testalle :: DefDec (SingleIn a) => [ZZZ a] -> ZZZ (Alle a)
testalle zs = ZZZ (AlleP defDec) (Alle (map _zzz2 zs)) (map _zzz3 zs) (concatMap _zzz4 zs)

testsome :: DefDec (SingleIn a) => [ZZZ a] -> ZZZ (Some 'False n a)
testsome zs = ZZZ (SomeP defDec) (Some (map _zzz2 zs)) (map _zzz3 zs) (concatMap _zzz4 zs)

testemos :: DefDec (SingleIn a) => [ZZZ a] -> ZZZ (Some 'True n a)
testemos zs = ZZZ (SomeP defDec) (Some (map _zzz2 zs)) (map _zzz3 zs) (concatMap _zzz4 zs)

testupd :: Int -> ZZZ Upd
testupd rc = ZZZ UpdP (Upd rc) rc []

testsel :: DefDec (Dec a) => [a] -> ZZZ (Sel a)
testsel as = ZZZ (SelP defDec) (Sel as) as []

testselone :: DefDec (Dec a) => a -> ZZZ (SelOne a)
testselone a = ZZZ (SelOneP defDec) (SelOne a) a []

testselraw :: [[SqlValue]] -> ZZZ SelRaw
testselraw as = ZZZ SelRawP (SelRaw as) as []

testselraw0 :: Rec ZZZ '[SelRaw]
testselraw0 = testselraw [[SqlString "asdf", SqlNull, SqlInt32 123], [SqlNull, SqlBool True, SqlInt32 9999]] :& RNil

testselraw1 :: Rec ZZZ '[SelRaw]
testselraw1 = testselraw [[SqlString "asdf", SqlNull, SqlInt32 123], [SqlNull, SqlBool True, SqlInt32 9999]] :& RNil

testselraw2 :: Rec ZZZ '[SelRaw]
testselraw2 = testselraw [[SqlString "asdf"]] :& RNil

testselraw3 :: Rec ZZZ '[SelRaw]
testselraw3 = testselraw [[SqlString "col1a", SqlString "col2a", SqlString "col3a"], [SqlString "col1b", SqlString "col2b", SqlString "col3b"]] :& RNil

testselraw4 :: Rec ZZZ '[SelRaw]
testselraw4 = testselraw (map (map SqlInt32) [[99,9999999],[1234,2],[12,2],[1,9999]]) :& RNil

testselraw5 :: Rec ZZZ '[SelRaw]
testselraw5 = testselraw (map (map SqlDouble) [[99,9999999],[12.34,2],[12,2],[1,9999.1234445]]) :& RNil

testselrawfail :: Rec ZZZ '[Sel [SqlValue]]
testselrawfail = testsel [[SqlString "asdf"]] :& RNil

testselraw6 :: Rec ZZZ '[SelRaw]
testselraw6 = testselraw [[SqlString "asdf", SqlNull, SqlInt32 123], [SqlNull, SqlBool True, SqlInt32 9999]] :& RNil

testp00 :: Rec ZZZ '[SelOne (Maybe Int)]
testp00 = testselone (Just 4) :& RNil

testp01 :: Rec ZZZ '[SelOne (One (Maybe Int))]
testp01 = testselone (One (Just 4)) :& RNil

testp0 :: Rec ZZZ '[Sel Int]
testp0 = testsel [4,5,6] :& RNil

testp1 :: Rec ZZZ '[SelOne Bool, SelOne Char]
testp1 = testselone True :& testselone 'c' :& RNil

testp1a :: Rec ZZZ '[SelOne (Int, String), SelOne (Int, String)]
testp1a = testselone (1,"hello\nworld and more data") :& testselone (2,"this is a test afield\r\n\tx\n\n\nxx") :& RNil

testp1b :: Rec ZZZ '[Sel (Int, String), Sel (Int, String)]
testp1b = testsel [(1,"hello\nworld and more data"),(2,"some text")] :& testsel [(99,"this is a test afield\r\n\tx\n\n\nxx"), (100,"some stuff")] :& RNil

testp1c :: Rec ZZZ '[Sel (String, Int), Sel (String, Int)]
testp1c = testsel [("hello\nworld and more data",1),("some text",2)] :& testsel [("this is a test afield\r\n\tx\n\n\nxx",99), ("some stuff",100)] :& RNil

testp2 :: Rec ZZZ '[Sel (One Bool), Sel (One Char)]
testp2 = testsel (map One [False,True]) :& testsel (map One ['c','d']) :& RNil

testp3 :: Rec ZZZ '[Sel (Bool, Char)]
testp3 = testsel [(False,'x'), (True,'y')] :& RNil

testp4 :: Rec ZZZ '[Sel (Bool, Char), Sel (Int, Bool, Char)]
testp4 = testsel [(False,'x'), (True,'y')] :& testsel [(1,False,'x'), (3,True,'y')] :& RNil

testp5 :: Rec ZZZ '[SelOne (Bool, Char), SelOne (Int, Bool, Char)]
testp5 = testselone (False,'x') :& testselone (1,True,'y') :& RNil

testp6 :: Rec ZZZ '[SelOne (Bool, Char), SelOne (String, Int), Sel (Double, Int,String)]
testp6 = testselone (False,'x') :& testselone ("hey man",123) :& testsel [(1.3,222,"aaa"),(1.4,333,"bbb"),(1.5,444,"ccc")] :& RNil

testp7 :: Rec ZZZ '[Sel (Int, Char), SelOne (String, Int), Sel (Double, Int, String)]
testp7 = testsel [(10,'x'),(11,'y'),(12,'z'),(13,'w')] :& testselone ("hey man",123) :& testsel [(1.3,222,"aaa"),(1.4,333,"bbb"),(1.5,444,"ccc"),(1.6,555,"ddd"),(1.7,666,"eee"),(1.8,777,"fff")] :& RNil

testp8 :: Rec ZZZ '[Sel (Int, Char), SelOne (String, Int), Upd, Sel (Double, Int, String)]
testp8 = testsel [(10,'x'),(11,'y'),(12,'z'),(13,'w')] :& testselone ("hey man",123) :& testupd 4949 :& testsel [(1.3,222,"aaa"),(1.4,333,"bbb"),(1.5,444,"ccc"),(1.6,555,"ddd"),(1.7,666,"eee"),(1.8,777,"fff")] :& RNil

testp9a :: Rec ZZZ '[Sel (Bool, Char) :+: Sel (Int, Bool, Char)]
testp9a = testeither False (testsel [(False,'x'), (True,'y')]) (testsel [(1,False,'x'), (3,True,'y')]) :& RNil

testp9b :: Rec ZZZ '[Sel (Bool, Char) :+: Sel (Int, Bool, Char)]
testp9b = testeither True (testsel [(False,'x'), (True,'y')]) (testsel [(1,False,'x'), (3,True,'y')]) :& RNil

testp10 :: Rec ZZZ '[Alle (Sel (Bool, Char))]
testp10 = testalle (replicate 4 (testsel [(False,'x'), (True,'y')])) :& RNil

testp11 :: Rec ZZZ '[Some 'False 3 (Sel (Bool, Char))]
testp11 = testsome (replicate 5 (testsel [(False,'x'), (True,'y')])) :& RNil

testp11a :: Rec ZZZ '[Some 'True 1 (Sel (Bool, Char)), SelOne (Char, Bool)]
testp11a = testemos (replicate 5 (testsel [(False,'x'), (True,'y')])) :& (testselone ('x', False)) :& RNil

-- this works even tho a single value but it is wrapped in T2
testp12 :: Rec ZZZ '[Sel T2]
testp12 = testsel [T2 11, T2 123] :& RNil

-- | test multiple values using generics-sop
testp12a :: Rec ZZZ '[Sel T4]
testp12a = testsel [T4 'x' 11, T4 'y' 123] :& RNil

testp12b :: Rec ZZZ '[Sel (Char, Double)]
testp12b = testsel [('x',11), ('y',123)] :& RNil

-- singles dont print without One
testp13 :: Rec ZZZ '[Sel (One Int)]
testp13 = testsel [One 11, One 123, One 1233] :& RNil

testp13a :: DefDec (Dec a) => [a] -> Rec ZZZ '[Sel (One a)]
testp13a xs = testsel (map One xs) :& RNil

testp14 :: Rec ZZZ '[SelRaw]
testp14 = testselraw [[SqlInt32 12,SqlBool False],[SqlInt32 12,SqlBool False]] :& RNil

newtype T2 = T2 { t22 :: Int } deriving (G.Generic, Show)
instance GS.Generic T2
instance GS.HasDatatypeInfo T2
instance DefDec (Dec T2) where
  defDec = T2 <$> defDec

newtype T3 = T3 { t33 :: Bool } deriving (G.Generic, Show)
instance GS.Generic T3
instance GS.HasDatatypeInfo T3
instance DefDec (Dec T3) where
  defDec = T3 <$> defDec

data T4 = T4 { t44 :: Char, t444 :: Double } deriving (G.Generic, Show)
instance GS.Generic T4
instance GS.HasDatatypeInfo T4
instance DefDec (Dec T4) where
  defDec = T4 <$> defDec <*> defDec


data T1 = T1 { ta1 :: String, ta2 :: Bool, ta3 :: String, ta4 :: Int, ta5 :: Char, ta6 :: Maybe Double } deriving (G.Generic, Show)
instance GS.Generic T1
instance GS.HasDatatypeInfo T1

t11,t12,t13 :: T1
t11 = T1 "hello" True "world" 123 'x' (Just 123.4556)
t12 = T1 "d"  False "hello there \nworld" (-123) 'x' (Just 123.4556)
t13 = T1 "hello" True s2a 99 'x' Nothing

s1, s2, s1a, s2a, s1b, s2b :: String
s1 = T.unpack [st|this is a long rant
of data with some interesting
stuff ansd this is the end of it|]

s2 = T.unpack [st|this is more data and here we go 1233444544
xxxxx                       yyyy
dide asef sadf sdf sdaf sdf|]

s1a = T.unpack [st|this is a long rant of data with some interesting stuff ansd this is the end of it|]

s2a = T.unpack [st|this is more data and here we go 1233444544 dide asef sadf sdf sdaf sdf|]

s1b = T.unpack [st|this is a long rant of data with some interesting stuff ansd this is the end of it|]

s2b = T.unpack [st|znork|]

t1s :: [Char -> Maybe Double -> T1]
t1s = [T1 s1 True s2 4444, T1 s1a False s2a 123, T1 s1b True s2b (-123)]

z1, z2, z2', z3, z4 :: IO ()
z1 = putStrLn $ tableString [def, numCol]
                       unicodeS
                       def
                       [ rowG ["Jack", "184.74"]
                       , rowG ["Jane", "162.2"]
                       ]

z2 = do
       let xx = top -- center -- vs top bottom
       putStrLn $ tableString [fixedLeftCol 50, def, fixedLeftCol 30, numCol]
                       asciiS
                       (titlesH ["title1", "Bool", "title2", "SomeNum"])
                       [ colsAllG xx [justifyText 50 s1,  ["True"], justifyText 50 ("afield\t\t" ++ s2 ++ "\t\txx"),  ["4444.1"]]
                       , colsAllG xx [justifyText 50 s1a,  ["False"], justifyText 50 s2a,  ["1.2"]]
                       , colsAllG xx [justifyText 50 s1b,  ["True"], justifyText 50 s2b,  ["2222229"]]
                       ]

z2' = do
       let xx = top -- center -- vs top bottom
       let zs = map (toRow (defTSimple FTrunc 40 400 defMorph1)) [t11, t12, t13]
       let cols = map (view _1) (toRow (defTSimple FTrunc 40 400 defMorph1) t11)
       let flds = getFieldNames (Proxy @T1)
       putStrLn $ tableString cols
                       asciiS
                       (titlesH flds)
                       (flip map zs $ \z -> colsAllG xx (map (view _2) z))


z2'' :: [T1] -> String
z2'' ts =
  let xx = top -- center -- vs top bottom
      zs = map (toRow (defTSimple FTrunc 40 400 defMorph1)) ts
      cols = map (view _1) (Prelude.head zs)
      flds = getFieldNames (Proxy @T1)
  in tableString cols
                 asciiS
                 (titlesH flds)
                 (flip map zs $ \z -> colsAllG xx (map (view _2) z))

z2''' :: forall xs a . (GS.HasDatatypeInfo a, GS.Code a ~ '[xs], GS.All FromField xs)
  => [a] -> String
z2''' ts =
  let xx = top -- center -- vs top bottom
      zs = map (toRow (defTSimple FTrunc 40 400 defMorph1)) ts
      cols = map (view _1) (Prelude.head zs)
      flds = getFieldNames (Proxy @a)
  in tableString cols
                 asciiS
                 (titlesH flds)
                 (flip map zs $ \z -> colsAllG xx (map (view _2) z))

-- have to get rid of newlines in the text hence using justifyText to do that
z3 = putStrLn $ tableString [fixedLeftCol 65, def, fixedLeftCol 30, numCol]
                       unicodeS
                       def
                       [ rowG [concat (justifyText 200 s1),  "True",  s2,  "4444.1"]
                       , rowG [s1a, "False", s2a, "299991.2"]
                       , rowG [s1b, "True",  s2b, "1.2"]
                       ]

txt1 :: String
txt1 = T.unpack [st|Lorem ipsum  dolor sit amet, consectetur adipisici
elit,  sed eiusmod  tempor incidunt  ut labore  et
dolore magna aliqua. Ut enim ad minim veniam, quis
nostrud   exercitation  ullamco  laboris  nisi  ut
aliquid  ex ea  commodi consequat.  Quis aute iure
reprehenderit   in  voluptate  velit  esse  cillum
dolore  eu fugiat  nulla pariatur.  Excepteur sint
obcaecat cupiditat non proident, sunt in culpa qui
officia deserunt mollit anim id est laborum.|]

z4 = putStrLn $ tableString [fixedLeftCol 50, numCol, numCol]
                          asciiS
                          (titlesH ["Text", "Length", "afield"])
                          [ colsAllG center [ justifyText 50 txt1
                                            , [show $ length txt1]
                                            , ["12345"]
                                            ]
                          ]

cols1 :: [ResultSet]
cols1 = [Right ([("SomeId",SqlColDesc {colType = SqlVarCharT, colSize = Just 20, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False}),("SerialNum",SqlColDesc {colType = SqlIntegerT, colSize = Just 10, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False}),("Guid",SqlColDesc {colType = SqlVarCharT, colSize = Just 100, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False}),("DateAdded",SqlColDesc {colType = SqlTimestampT, colSize = Just 23, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False}),("SomeId2",SqlColDesc {colType = SqlVarCharT, colSize = Just 20, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just True})]
      ,[[SqlByteString "1234\n\r\n\n5678",SqlInt32 12345,SqlByteString "888A8AA8-AAAA-BBBB-CCCC--A1111AA1A11A",SqlLocalTime (read "2016-04-18 14:33:12.54"),SqlByteString "012345678"]
      ,[SqlByteString "23456788",SqlInt32 12346,SqlByteString "888A8AA8-AAAA-BBBB-CCCC--A1111AA1A11A",SqlLocalTime (read "2016-04-18 14:33:12.54"),SqlByteString "023456788"]
      ,[SqlByteString "28763335",SqlInt32 12347,SqlByteString "888A8AA8-AAAA-BBBB-CCCC--A1111AA1A11A",SqlLocalTime (read "2016-04-18 14:33:12.54"),SqlByteString "028763335"]
      ,[SqlByteString "34567889",SqlInt32 12348,SqlByteString "888A8AA8-AAAA-BBBB-CCCC--A1111AA1A11A",SqlLocalTime (read "2016-04-18 14:33:12.54"),SqlByteString "034567889"]
      ,[SqlByteString "45678890",SqlInt32 12349,SqlByteString "888A8AA8-AAAA-BBBB-CCCC--A1111AA1A11A",SqlLocalTime (read "2016-04-18 14:33:12.54"),SqlByteString "045678890"]
      ,[SqlByteString "567889012",SqlInt32 23491,SqlByteString "888A8AA8-AAAA-BBBB-CCCC--A1111AA1A11A",SqlLocalTime (read "2016-04-18 14:33:12.54"),SqlByteString "567889012"]
      ,[SqlByteString "67889012",SqlInt32 23492,SqlByteString "888A8AA8-AAAA-BBBB-CCCC--A1111AA1A11A",SqlLocalTime (read "2016-04-18 14:33:12.54"),SqlByteString "067889012"]
      ,[SqlByteString "78890123",SqlInt32 23493,SqlByteString "888A8AA8-AAAA-BBBB-CCCC--A1111AA1A11A",SqlLocalTime (read "2016-04-18 14:33:12.54"),SqlByteString "078890123"]
      ,[SqlByteString "88901234",SqlInt32 23494,SqlByteString "888A8AA8-AAAA-BBBB-CCCC--A1111AA1A11A",SqlLocalTime (read "2016-04-18 14:33:12.54"),SqlByteString "088901234"]
      ,[SqlByteString "890123456",SqlInt32 23495,SqlByteString "888A8AA8-AAAA-BBBB-CCCC--A1111AA1A11A",SqlLocalTime (read "2016-04-18 14:33:12.54"),SqlByteString "890123456"]])
      ]

cols2 :: [ResultSet]
cols2 = [Left 123, Left 456]

type TF0 = F '["aa" ::: String]

tf00 :: Rec ZZZ '[Sel TF0]
tf00 = ZZZ (SelP defDec) undefined [#aa =: "afield" :& RNil, #aa =: "this" :& RNil, #aa =: "world" :& RNil] [] :& RNil

type TF1 = F '["aa" ::: String, "bb" ::: Int]

tf11 :: Rec ZZZ '[Sel TF1]
tf11 = ZZZ (SelP defDec) undefined [#aa =: "afield" :& #bb =: 999 :& RNil] [] :& RNil

type TF2 = F '["aa" ::: (Int, String), "bb" ::: (String,String,String)]

tf11x :: Rec ZZZ '[Sel TF2]
tf11x = ZZZ (SelP defDec) undefined [#aa =: (44, "afield") :& #bb =: ("a","b","c") :& RNil] [] :& RNil

tf22 :: Rec ZZZ '[Sel (String,Int)]
tf22 = ZZZ (SelP defDec) undefined [("afield",999)] [] :& RNil

tf33 :: Rec ZZZ '[Sel (MakeF (String,Int))]
tf33 = ZZZ (SelP defDec) undefined [#c1 =: "afield" :& #c2 =: 999 :& RNil, #c1 =: "afield2" :& #c2 =: 1000 :& RNil] [] :& RNil

tf11a :: Rec ZZZ '[Sel TF1]
tf11a = ZZZ (SelP defDec) undefined [#aa =: "afield" :& #bb =: 999 :& RNil, #aa =: "fred" :& #bb =: (-12) :& RNil] [] :& RNil

tf11b :: Rec ZZZ '[SelOne TF1]
tf11b = ZZZ (SelOneP defDec) undefined (#aa =: "afield" :& #bb =: 999 :& RNil) [] :& RNil

tf22b :: Rec ZZZ '[SelOne (String,Int)]
tf22b = ZZZ (SelOneP defDec) undefined ("afield",999) [] :& RNil

tf33b :: Rec ZZZ '[SelOne (MakeF (String,Int))]
tf33b = ZZZ (SelOneP defDec) undefined (#c1 =: "afield" :& #c2 =: 999 :& RNil) [] :& RNil

data IP a = IP {_octet1 :: a, _octet2 :: a, _octet3 :: a, _octet4 :: a} deriving (Show,Read,Eq)

instance Show a => FromField (IP a) where
  fromField = (:[]) . show
  coltype i = const [upto i]
  fieldtype _ = const [Stringy]


