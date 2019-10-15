{-# OPTIONS -Wall -Wcompat -Wincomplete-record-updates -Wincomplete-uni-patterns -Wredundant-constraints #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoStarIsType #-}
module TestSql where
import Data.Vinyl
import Data.Vinyl.TypeLevel hiding (Nat)
import Control.Lens hiding (rmap,Identity,Const)
import Data.Text (Text)
import Sql
import TablePrinter
import PredState hiding (pe,pe2)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Hspec
import Database.HDBC.ColTypes (SqlTypeId (SqlUnknownT))
import GHC.TypeLits (KnownNat)
import Control.Arrow
import Predicate
import Refined3
import Refined3Helper
import Refined

data R1 = R1 { r1 :: String, r2 :: Bool, r3 :: Char } deriving (Show,Eq)

decR1 :: Dec R1
decR1 = R1 <$> defDec <*> defDec <*> defDec

instance DefDec (Dec R1) where
  defDec = decR1

encR1 :: Enc R1
encR1 = Enc $ \(R1 x y z) -> [SqlString x, SqlBool y, SqlChar z]

instance DefEnc (Enc R1) where
  defEnc = encR1

{-
>(unDec @R1) defDec $ unEnc defEnc $ R1 "abc" True 'x'
Right (R1 {r1 = "abc", r2 = True, r3 = 'x'},[])
it :: Either DE (R1, [SqlValue])
-}

-- anyOf (traverse . suShort) (=="Update") (xes @UnexpectedResultSetTypeE e)
suite :: IO ()
suite = defaultMain $ testGroup "TestSql"
  [ testCase "simpleret1" $ (@?=) (ext <$> processRetCol (E1 (SelOneP @Bool ptrue defDec)) [Right ([], [[SqlBool True]])]) (Right True)
  , testCase "simpleret2" $ (@?=) (ext <$> processRetCol (E2 (SelP @Bool ptrue defDec) (UpdP ptrue)) [Right ([], [[SqlBool False],[SqlBool True]]), Left 23]) (Right ([False,True],23))
  , testCase "single.fail1" $ assertBool "a1" (hasError @SingleColE (processRetCol (E1 (SelOneP @Bool ptrue defDec)) [Right ([], [])]))
  , testCase "single.fail2" $ assertBool "a2" (hasError @UnexpectedResultSetTypeE (processRetCol (E1 (SelOneP @Bool ptrue defDec)) [Left 4]))  -- (Left "SelOne ResultSet 2:Single (SelOne a):expected 1 row but found 0 xxs=[]")
  , testCase "single.fail3" $ assertBool "a3" (hasError @SingleColE (processRetCol (E1 (SelOneP @Bool ptrue defDec)) [Right ([], [])])) -- (Left "SelOne ResultSet 2:Single (SelOne a):expected 1 row but found 0 xxs=[]")
  , testCase "encodemultiple" $ (@?=) (encodeVals (E2 (defEnc @(Enc Int)) (defEnc @(Enc (Bool,String)))) (I2 1 (True,"xxx"))) [SqlInt32 1,SqlInt32 1,SqlString "xxx"]
  , testCase "encodesingle" $ (@?=) (encodeVals (E1 (defEnc @(Enc (Int, (Bool,String))))) (I1 (1,(True,"xxx")))) [SqlInt32 1,SqlInt32 1,SqlString "xxx"]
  , testCase "single.fail4" $ assertBool "a4" (hasError @ConvE (processRetCol (E1 (SelOneP @Bool ptrue defDec)) [Right ([], [[SqlInt32 112]])]))
  , testCase "single.fail5" $ assertBool "a5" (anyOf (_Left . to (xes @ConvE) . traverse . to _cvType) (=="Bool") (processRetCol (E1 (SelOneP @Bool ptrue defDec)) [Right ([], [[SqlInt32 112]])]))
  , testCase "single.fail6" $ assertBool "a6" (hasn't (_Left . to (xes @UnexpectedResultSetTypeE) . _Empty) (processRetCol (E1 (SelOneP @Bool ptrue defDec)) [Left 4]))  -- (Left "SelOne ResultSet 2:Single (SelOne a):expected 1 row but found 0 xxs=[]")
  , testCase "single.fail7" $ assertBool "a7" (has (_Left . to (xes @UnexpectedResultSetTypeE) . _head) (processRetCol (E1 (SelOneP @Bool ptrue defDec)) [Left 4]))  -- (Left "SelOne ResultSet 2:Single (SelOne a):expected 1 row but found 0 xxs=[]")
  , testCase "gtest1" $ assertBool "a8" (xes'' @NoResultSetE gtest1)
  , testCase "gtest2" $ (@?=) (ext <$> gtest2) (Right (123,[(11,True),(22,False)]))
  , testCase "gtest3" $ (@?=) (ext <$> gtest3) (Right (123,[("afield",True),("zzz",False)]))
  , testCase "gtest4" $ (@?=) (ext <$> gtest4) (Right [123,999,-8])
  , testCase "gtest5" $ (@?=) (ext <$> gtest5) (Right (Left 123))
  , testCase "gtest6" $ (@?=) (ext <$> gtest5') (Right (Right 123))
  , testCase "gtest7" $ assertBool "a9" (xes'' @UnexpectedResultSetTypeE gtest5'')
  , testCase "gtest8" $ assertBool "a10" (xes'' @UnconsumedColE gtest5''')
--  , testCase "gtest9" $ assertBool (xes'' @NoResultSetE gtest5'''')

  ]
{- these 3 are the same
hasn't (_Left . to (xes @UnexpectedResultSetTypeE) . _Empty
has (_Left . to (xes @UnexpectedResultSetTypeE) . _head)
-}
spec :: SpecWith ()
spec =
  describe "Type Tests" $ do
    it "should allow combined Some and Alle with no Alle data" $
      ext <$> processRetCol valid1 [Left 123, Left 999] `shouldBe` Right ([123,999],[])
--    it "should fail cos expecting an Upd but got a select type" $
--      processRetCol valid1 [Right [[]]] `shouldBe` Left ()
    it "should allow combined Some and Alle with Alle data" $
      ext <$> processRetCol valid1 [Left 123,Left 999, Right ([], [[SqlBool True]]), Right ([], [[SqlBool False]])] `shouldBe` Right ([123,999],[True,False])
    it "should allow Upd and SelOne " $
      ext <$> processRetCol valid3 [Left 123,Right ([], [[SqlInt32 3]])] `shouldBe` Right (123,3)
    it "should allow :+: without any Alle data" $
      ext <$> processRetCol valid2 [Left 123] `shouldBe` Right (Left 123)
    it "should allow :+: without any Alle data" $
      ext <$> processRetCol valid2 [Right ([], [[SqlInt32 3]])] `shouldBe` Right (Right 3)
    it "tst2' good" $
      ext' <$> tst2' @4 2 1 `shouldBe` Right ([(True,'x'),(False,'y')], (4, ([R1 "xx" True 'a'],())))
    it "tst2' fail" $
      ext' <$> tst2' @3 2 1 `shouldSatisfy` (Left 2==) . left (length . xes @SingleColE)
    it "tst21' good" $
      ext' <$> tst21' @3 `shouldBe` Right ([5,12,7,3,4,99,22],())


doit :: IO ()
doit = do
 hspec $
  describe "Type Tests" $ do
    it "should allow combined Some and Alle with no Alle data" $
      ext <$> processRetCol valid1 [Left 123, Left 999] `shouldBe` Right ([123,999],[])
--    it "should fail cos expecting an Upd but got a select type" $
--      processRetCol valid1 [Right [[]]] `shouldBe` Left ()
    it "should allow combined Some and Alle with Alle data" $
      ext <$> processRetCol valid1 [Left 123,Left 999, Right ([], [[SqlBool True]]), Right ([], [[SqlBool False]])] `shouldBe` Right ([123,999],[True,False])
    it "should allow Upd and SelOne " $
      ext <$> processRetCol valid3 [Left 123,Right ([], [[SqlInt32 3]])] `shouldBe` Right (123,3)
    it "should allow :+: without any Alle data" $
      ext <$> processRetCol valid2 [Left 123] `shouldBe` Right (Left 123)
    it "should allow :+: without any Alle data" $
      ext <$> processRetCol valid2 [Right ([], [[SqlInt32 3]])] `shouldBe` Right (Right 3)

valid1 :: Rec SingleIn '[Some 'False 2 Upd, Alle (SelOne Bool)]
valid1 = E2 (SomeP defDec ptrue) (AlleP defDec ptrue)

valid2 :: Rec SingleIn '[Upd :+: SelOne Int]
valid2 = E1 (UpdP ptrue :+: SelOneP ptrue defDec)

valid3 :: Rec SingleIn '[Upd, SelOne Int]
valid3 = E2 (UpdP ptrue) (SelOneP ptrue defDec)

rsa :: ResultSet
rsa = Right ([], [[SqlBool True,SqlChar 'x'],[SqlBool False,SqlChar 'y']])

rsb :: Either Int a
rsb = Left 4

rsc :: ResultSet
rsc = Right ([], [[SqlString "xx",SqlBool True,SqlChar 'a']])

tst1 :: Either SE (Rec ZZZ '[Sel (Bool, Char), Upd, Sel R1])
tst1 = processRetCol (E3 (SelP ptrue defDec) (UpdP ptrue) (SelP ptrue defDec)) [rsa,rsb,rsc]

tst2 :: Int -> Int -> Int -> Either SE (Rec ZZZ '[Sel (Bool, Char), Upd, Sel R1])
tst2 i j k = processRetCol (E3 (SelP (PLen (peq i)) defDec) (UpdP (peq j)) (SelP (PLen (peq k)) defDec)) [rsa,rsb,rsc]

-- test type level predicate for update: expected @4
tst2' :: forall n. KnownNat n => Int -> Int -> Either SE (Rec ZZZ '[Sel (Bool, Char), UpdN 'OPEQ n, Sel R1])
tst2' i k = processRetCol (E3 (SelP (PLen (peq i)) defDec) defDec (SelP (PLen (peq k)) defDec)) [rsa,rsb,rsc]

-- predicate on length of results and n=0,1,2,3 work but nothing above that
tst21' :: forall n. KnownNat n => Either SE (Rec ZZZ '[Alle (UpdN 'OPGE n)])
tst21' = processRetCol (E1 (AlleP defDec (PLen (pgt 4)))) [Left 5, Left 12, Left 7, Left 3, Left 4, Left 99, Left 22]

-- UGE n == UpdN 'OPGE n
tst21'' :: forall n. KnownNat n => Either SE (Rec ZZZ '[Alle (UGE n)])
tst21'' = processRetCol (E1 (AlleP defDec (PLen (pgt 4)))) [Left 5, Left 12, Left 7, Left 3, Left 4, Left 99, Left 22]

-- invalid string for sqlchar
tst3 :: Either SE (Rec ZZZ '[Sel (Bool, String), Upd, Sel R1])
tst3 = processRetCol (E3 (SelP ptrue defDec) (UpdP ptrue) (SelP ptrue defDec)) [rsa,rsb,rsc]

tst4r :: (Integral i, Integral j) => i -> j -> Either SE (Rec ZZZ '[Sel (Text, Refined (Between 4 10 && Id /= 7) Int, Double)])
tst4r i j = processRetCol (E1 (SelP ptrue defDec)) [Right ([], [[SqlString "abc", SqlInt32 (fromIntegral i), SqlDouble 1.2], [SqlString "abc", SqlInt32 (fromIntegral j), SqlDouble 1.2]])]

tst4r1 :: (Integral i, Integral j) => i -> j -> Either SE (Rec ZZZ '[Sel (Text, Refined (Guard (Printf "oops val=%03d") (Between 4 10 && Id /= 7) >> 'True) Int, Double)])
tst4r1 i j = processRetCol (E1 (SelP ptrue defDec)) [Right ([], [[SqlString "abc", SqlInt32 (fromIntegral i), SqlDouble 1.2], [SqlString "abc", SqlInt32 (fromIntegral j), SqlDouble 1.2]])]

tst3r :: Either SE (Rec ZZZ '[Sel (One String)])
tst3r = processRetCol (E1 (SelP ptrue defDec)) [Right ([], [[SqlString "123"]])]

-- tst3r1 "1230" checkdigit succeeds
tst3r1 :: String -> Either SE (Rec ZZZ '[Sel (One (LuhnR 4))])
tst3r1 s = processRetCol (E1 (SelP ptrue defDec)) [Right ([], [[SqlString s]])]

tst3r2 :: Either SE (Rec ZZZ '[Sel (One (LuhnR 4))])
tst3r2 = processRetCol (E1 (SelP ptrue defDec)) [Right ([], [[SqlInt32 111]])]

tst3r3 :: String -> Either SE (Rec ZZZ '[SelOne (One (Refined3 (ReadP Int) (Gt 4) (Exitwhen (Printf "Bad output=%d") (Gt 10) >> ShowP ) String))])
tst3r3 s = processRetCol (E1 (SelOneP ptrue defDec)) [Right ([], [[SqlString s]])]

tst3rgood :: Either SE (Rec ZZZ '[Sel (One (Refined3 (ReadP Int) (Gt 4) ShowP String))])
tst3rgood = processRetCol (E1 (SelP ptrue defDec)) [Right ([], [[SqlString "123"]])]

tst3rbad :: Either SE (Rec ZZZ '[Sel (One (Refined3 (ReadP Int) (Gt 4) ShowP String))])
tst3rbad = processRetCol (E1 (SelP ptrue defDec)) [Right ([], [[SqlString "-123"]])]
{-
>tst3rgood
Right {ZZZ {_zzz1 = SelP PConst TrueP
 Dec<fn>, _zzz2 = Sel {unSel = [One {unOne = Refined3 {in3 = 123, out3 = "123"}}]}, _zzz3 = [One {unOne = Refined3 {in3 = 123, out3 = "123"}}], _zzz4 = []}}
it ::
  Either
    SE (Rec ZZZ '[Sel (One (Refined3 (ReadP Int) (Gt 4) ShowP String))])

>tst3rbad
Left ((Col SingleColE {_siInstance = "Sel", _siPos = Just 0, _siMessage = "", _siRss = [Right [[SqlString "-123"]]]}) :| [(
Col DecodingE method=decRefined3 msg=failed e=boolean check false
[Node {rootLabel = PE {_peBoolP = PresentP, _peStrings = ["ReadP Int (-123) -123 | -123"]}, subForest = [Node {rootLabel
= PE {_peBoolP = PresentP, _peStrings = ["Id \"-123\""]}, subForest = []}]},Node {rootLabel = PE {_peBoolP = FalseP, _pe
Strings = ["CMP -123 > 4"]}, subForest = [Node {rootLabel = PE {_peBoolP = PresentP, _peStrings = ["I"]}, subForest = []
},Node {rootLabel = PE {_peBoolP = PresentP, _peStrings = ["'4"]}, subForest = []}]}]
 sqlvalues=)])
it ::
  Either
    SE (Rec ZZZ '[Sel (One (Refined3 (ReadP Int) (Gt 4) ShowP String))])
-}

tst4 :: Int -> Either SE (Rec ZZZ '[Sel (Bool, Char)])
tst4 i = processRetCol (E1 (SelP (PLen (peq i)) defDec)) [rsa]

gtestA :: (ValidateNested rs, RecAll ZZZ rs SingleZ) => [ResultSet] -> Rec SingleIn rs -> Either SE (Rec ZZZ rs)
gtestA a b = processRetCol b a

gtest1, gtest2, gtest2' :: Either SE (Rec ZZZ '[Upd, Sel (Int, Bool)])
gtest1 = gtestA [Left 123] defDec
gtest2 = gtestA [Left 123, Right ([], [[SqlInt32 11, SqlBool True],[SqlInt32 22, SqlBool False]])] defDec
gtest2' = gtestA [Left 123, Right ([], [[SqlInt32 11, SqlBool True],[SqlInt32 22, SqlBool False]])] (ueq 124 :& defDec :& RNil)

gtest3 :: Either SE (Rec ZZZ '[Upd, Sel (Text, Bool)])
gtest3 = gtestA [Left 123, Right ([], [[SqlString "afield", SqlBool True],[SqlString "zzz", SqlBool False]])] defDec

gtest4 :: Either SE (Rec ZZZ '[Alle Upd])
gtest4 = gtestA [Left 123, Left 999, Left (-8)] defDec

gtest5, gtest5', gtest5'', gtest5''', gtest5'''' :: Either SE (Rec ZZZ '[Upd :+: SelOne Int])
gtest5 = gtestA [Left 123] defDec
gtest5' = gtestA [Right ([], [[SqlInt32 123]])] defDec
gtest5'' = gtestA [Right ([], [[SqlInt32 123], [SqlInt32 456]])] defDec -- fails cos too many rows
gtest5''' = gtestA [Right ([], [[SqlInt32 123, SqlInt32 456]])] defDec -- fails cos too many values
gtest5'''' = gtestA [Right ([], [])] defDec -- fails cos not enough rows

gtest6, gtest6', gtest6'', gtest6''', gtest6'''' :: Either SE (Rec ZZZ '[SelOne Int :+: Upd])
gtest6 = gtestA [Right ([], [[SqlInt32 123]])] defDec
gtest6' = gtestA [Left 4] defDec
gtest6'' = gtestA [Right ([], [[SqlInt32 123], [SqlInt32 456]])] defDec -- fails cos too many rows
gtest6''' = gtestA [Right ([], [[SqlInt32 123, SqlInt32 456]])] defDec -- fails cos too many values
gtest6'''' = gtestA [Right ([], [])] defDec -- fails cos not enough rows


x1 :: SingleIn Upd
x1 = UpdP ptrue

x1' :: SingleIn Upd
x1' = UpdP ptrue

x1'' :: SingleIn (Alle Upd)
x1'' = AlleP (UpdP ptrue) ptrue

x1''' :: SingleIn (Upd :+: SelOne Int)
x1''' = UpdP 1 :+: SelOneP 1 defDec

x1'''' :: SingleIn (SelOne Int :+: Upd)
x1'''' = SelOneP 1 defDec :+: UpdP 1

x2 :: SingleIn (Sel (Int, Bool))
x2 = SelP ptrue defDec

x2' :: SingleIn (Sel (Int, Bool))
x2' = SelP ptrue defDec

x3' :: SingleIn (Sel (Text, Bool))
x3' = SelP ptrue defDec

validx1 :: Rec SingleIn '[Upd :+: SelOne Int]
validx1 = E1 (UpdP ptrue :+: SelOneP ptrue defDec)

validx2 :: Rec SingleIn '[SelOne Int]
validx2 = E1 (SelOneP ptrue defDec)

validx3 :: Rec SingleIn '[Upd, Alle Upd]
validx3 = E2 (UpdP ptrue) (AlleP (UpdP ptrue) ptrue)

validx4 :: Rec SingleIn '[Upd, Upd, Sel (Double,Int)]
validx4 = E3 (UpdP ptrue) (UpdP ptrue) (SelP ptrue defDec)

s0 :: Sql db '[Int] '[Upd]
s0 = Sql "zzz" (defEnc :& RNil) (x1 :& RNil) "s0"

s1 :: Sql db '[Int,Double] '[Upd, Sel (Int, Bool)]
s1 = Sql "abc" (defEnc :& defEnc :& RNil) (x1' :& x2' :& RNil) "s1"

-- same as s1 but dont need to specify each defEnc
s1a :: Sql db '[Int,Double] '[Upd, Sel (Int, Bool)]
s1a = Sql "abc" defEnc (x1' :& x2' :& RNil) "s1"

-- Alle is not last but gets detected when used with runSql
s2 :: Sql db '[] '[Alle Upd, Upd :+: SelOne Int]
s2 = Sql "def" RNil (x1'' :& x1''' :& RNil) "s2"

s3 :: Sql db '[] '[Upd :+: SelOne Int, Alle Upd]
s3 = Sql "def" RNil (x1''' :& x1'' :& RNil) "s3"

s4 :: Sql db '[Bool,Text] '[Upd :+: SelOne Int, SelOne Char, Alle (Sel Int)]
s4 = Sql "def" (defEnc :& defEnc :& RNil) (UpdP (peq 3) :+: SelOneP ptrue defDec
                  :& SelOneP (PElem ['a'..'h']) defDec
                  :& AlleP defDec (PNot PNull)
                  :& RNil) "s4"

gtest0 :: Rec ZZZ '[SelOne (F '["aa" ::: Int])]
gtest0 = ZZZ (SelOneP ptrue defDec) (SelOne ((#aa =: 123) :& RNil)) ((#aa =: 123) :& RNil) [] :& RNil

gtest00 :: Rec ZZZ '[SelOne (F '["aa" ::: Int, "bb" ::: Bool])]
gtest00 = ZZZ (SelOneP ptrue defDec) (SelOne ((#aa =: 123) :& (#bb =: True) :& RNil)) ((#aa =: 123) :& (#bb =: True) :& RNil) [] :& RNil

gtest000 :: Rec ZZZ '[Sel (F '["aa" ::: Int, "bb" ::: Bool])]
gtest000 = ZZZ (SelP ptrue defDec) (Sel [(#aa =: 123) :& (#bb =: True) :& RNil]) [(#aa =: 123) :& (#bb =: True) :& RNil] [] :& RNil

gtest0000 :: Rec ZZZ '[Sel (F '["aa" ::: Int, "bb" ::: Bool])]
gtest0000 = ZZZ (SelP ptrue defDec) (Sel [(#aa =: 123) :& (#bb =: True) :& RNil, (#aa =: 321) :& (#bb =: False) :& RNil]) [(#aa =: 123) :& (#bb =: True) :& RNil, (#aa =: 321) :& (#bb =: False) :& RNil] [] :& RNil

ys' :: Rec SingleIn '[Upd, SelRaw, Alle (SelOne Int)]
ys' = UpdP (peq 0) :& SelRawP PNull :& AlleP (SelOneP (peq @Int 0) (defDec @(Dec Int))) PNull :& RNil

-- should always fail cos has an alle but not at the end
zs' :: Rec SingleIn '[Upd, SelRaw, Alle (SelOne Int), Alle Upd]
zs' = UpdP (peq 0) :& SelRawP PNull :& AlleP (SelOneP (peq @Int 0) (defDec @(Dec Int))) PNull :& AlleP (UpdP (peq 0)) PNull :& RNil

testColDesc :: String -> (String, SqlColDesc)
testColDesc s = (s, SqlColDesc (SqlUnknownT "for testing sql.hs") (Just 5) (Just 7) (Just 11) (Just True))

tt1' :: [ResultSet]
tt1' = [Left 3, Right ([], [[SqlChar 'c']]), Right ([], [[SqlInt32 12],[SqlInt32 456],[SqlInt32 999]])]

tt2' :: [ResultSet]
tt2' = [Left 3, Right ([testColDesc "a1"], [[SqlChar 'c']]), Right ([testColDesc "b1"], [[SqlInt32 12],[SqlInt32 456],[SqlInt32 999]])]

t11 :: ((
        SingleOut (Alle (Upd :+: Sel Int)) ~ [Either Int [Int]]
       ,SingleOut (SelOne Bool :+: Sel Int) ~ Either Bool [Int]
       ,SingleOut Upd ~ Int
       ,SingleOut (Sel (Bool,String)) ~ [(Bool, String)]
       ,SingleOut (SelOne (Bool,String)) ~ (Bool, String)
       ,SingleOuts '[Upd,Upd,Sel (Bool,String)] ~ (Int, Int, [(Bool, String)])
       ,SingleOut (Sel (MakeF (Bool,String))) ~ [Rec ElField '["c1" ::: Bool, "c2" ::: String]]
       ,SingleOut (Sel (MakeF' '[Bool,String])) ~ [Rec ElField '["c1" ::: Bool, "c2" ::: String]]
       ,SingleOut (Sel (F '["abool" ::: Bool, "astring" ::: String])) ~ [Rec ElField '["abool" ::: Bool, "astring" ::: String]]
       ,SingleOut (Alle (Upd :+: SelRaw)) ~ [Either Int [[SqlValue]]]

       ) => ()) -> ()
t11 x = x


t12 :: Sql db '[Int] '[UpdN 'OPEQ 0, Sel Int, SelOne Bool]
t12 =
  let _a :: Text
      _b :: Rec Enc '[Int]
      _c :: Rec SingleIn '[UpdN 'OPEQ 0, Sel Int, SelOne Bool]
      _d :: Text
      z@(Sql _a _b _c _d) = sqlCombine (mkSql @'[U0] @'[] "[sqlA]" "select 1") (mkSql @'[Sel Int,SelOne Bool] @'[Int] "[sqlB]" "select 2")
  in z