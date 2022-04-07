{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module TestSql where

import Control.Arrow
import Control.Lens hiding (Const, Identity, rmap)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Vinyl
import Database.HDBC.ColTypes (SqlTypeId (SqlUnknownT))
import GHC.TypeLits (KnownNat)
import HSql.Core
import HSql.Core.VinylUtils
import TestSqlHelper

{-
import Predicate
import Predicate.Examples.Refined3
import Predicate.Refined3
-}
import Test.Hspec
import Test.Tasty
import Test.Tasty.HUnit

data R1 = R1 {r1 :: !String, r2 :: !Bool, r3 :: !Char} deriving stock (Show, Eq)

decR1 :: Dec R1
decR1 = R1 <$> defDec <*> defDec <*> defDec

instance DefDec (Dec R1) where
  defDec = decR1

encR1 :: Enc R1
encR1 = Enc $ \(R1 x y z) -> [SqlString x, SqlBool y, SqlChar z]

instance DefEnc (Enc R1) where
  defEnc = encR1

-- anyOf (traverse . suShort) (=="Update") (xes @UnexpectedResultSetTypeE e)
suite :: TestTree
suite =
  testGroup
    "TestSql"
    [ testCase "simpleret1" $ (@?=) (ext <$> processRetCol (E1 (SelRowP @Bool defDec)) [Right ([], [[SqlBool True]])]) (Right True)
    , testCase "simpleret2" $ (@?=) (ext <$> processRetCol (E2 (SelP @Bool defDec) UpdP) [Right ([], [[SqlBool False], [SqlBool True]]), Left 23]) (Right ([False, True], 23))
    , testCase "single.fail1" $ assertBool "a1" (hasError @SqlE (processRetCol (E1 (SelRowP @Bool defDec)) [Right ([], [])]))
    , testCase "single.fail2" $ assertBool "a2" (hasError @UnexpectedResultSetTypeE (processRetCol (E1 (SelRowP @Bool defDec)) [Left 4])) -- (Left "SelRow ResultSet 2:Single (SelRow a):expected 1 row but found 0 xxs=[]")
    , testCase "single.fail3" $ assertBool "a3" (hasError @SqlE (processRetCol (E1 (SelRowP @Bool defDec)) [Right ([], [])])) -- (Left "SelRow ResultSet 2:Single (SelRow a):expected 1 row but found 0 xxs=[]")
    , testCase "encodemultiple" $ (@?=) (encodeVals (E2 (defEnc @(Enc Int)) (defEnc @(Enc (Bool, String)))) (I2 1 (True, "xxx"))) [SqlInt32 1, SqlInt32 1, SqlString "xxx"]
    , testCase "encodesingle" $ (@?=) (encodeVals (E1 (defEnc @(Enc (Int, (Bool, String))))) (I1 (1, (True, "xxx")))) [SqlInt32 1, SqlInt32 1, SqlString "xxx"]
    , testCase "single.fail4" $ assertBool "a4" (hasError @ConvE (processRetCol (E1 (SelRowP @Bool defDec)) [Right ([], [[SqlInt32 112]])]))
    , testCase "single.fail5" $ assertBool "a5" (anyOf (_Left . to (xes @ConvE) . traverse . to cvType) (== "Bool") (processRetCol (E1 (SelRowP @Bool defDec)) [Right ([], [[SqlInt32 112]])]))
    , testCase "single.fail6" $ assertBool "a6" (hasn't (_Left . to (xes @UnexpectedResultSetTypeE) . _Empty) (processRetCol (E1 (SelRowP @Bool defDec)) [Left 4])) -- (Left "SelRow ResultSet 2:Single (SelRow a):expected 1 row but found 0 xxs=[]")
    , testCase "single.fail7" $ assertBool "a7" (has (_Left . to (xes @UnexpectedResultSetTypeE) . _head) (processRetCol (E1 (SelRowP @Bool defDec)) [Left 4])) -- (Left "SelRow ResultSet 2:Single (SelRow a):expected 1 row but found 0 xxs=[]")
    , testCase "gtest1" $ assertBool "a8" (xes'' @NoResultSetE gtest1)
    , testCase "gtest2" $ (@?=) (ext <$> gtest2) (Right (123, [(11, True), (22, False)]))
    , testCase "gtest3" $ (@?=) (ext <$> gtest3) (Right (123, [("afield", True), ("zzz", False)]))
    , testCase "gtest4" $ (@?=) (ext <$> gtest4) (Right [123, 999, -8])
    , testCase "gtest5" $ (@?=) (ext <$> gtest5) (Right (Left 123))
    , testCase "gtest6" $ (@?=) (ext <$> gtest5A) (Right (Right 123))
    , testCase "gtest7" $ assertBool "a9" (xes'' @UnexpectedResultSetTypeE gtest5B)
    , testCase "gtest8" $ assertBool "a10" (xes'' @UnconsumedColumnE gtest5C)
    --  , testCase "gtest9" $ assertBool (xes'' @NoResultSetE gtest5D)
    ]

spec :: SpecWith ()
spec =
  describe "Type Tests" $ do
    it "should allow combined Exact and Alle with no Alle data" $
      ext <$> processRetCol @'[Exact 2 Upd, Alle (SelRow Bool)] defDec [Left 123, Left 999]
        `shouldBe` Right (123 :| [999], [])
    --    it "should fail cos expecting an Upd but got a select type" $
    --      processRetCol @'[Exact 2 Upd, Alle (SelRow Bool)] defDec [Right [[]]] `shouldBe` Left ()
    it "should allow combined Exact and Alle with Alle data" $
      ext <$> processRetCol @'[Exact 2 Upd, Alle (SelRow Bool)] defDec [Left 123, Left 999, Right ([], [[SqlBool True]]), Right ([], [[SqlBool False]])]
        `shouldBe` Right (123 :| [999], [True, False])
    it "should allow Upd and SelRow " $
      ext <$> processRetCol @'[Upd, SelRow Int] defDec [Left 123, Right ([], [[SqlInt32 3]])] `shouldBe` Right (123, 3)
    it "should allow :+: without any Alle data" $
      ext <$> processRetCol @'[Upd :+: SelRow Int] defDec [Left 123] `shouldBe` Right (Left 123)
    it "should allow :+: without any Alle data" $
      ext <$> processRetCol @'[Upd :+: SelRow Int] defDec [Right ([], [[SqlInt32 3]])] `shouldBe` Right (Right 3)
    it "tst2' good" $
      ext' <$> tst2' @4 `shouldBe` Right ([(True, 'x'), (False, 'y')], (4, ([R1 "xx" True 'a'], ())))
    it "tst2' fail" $
      ext' <$> tst2' @3 `shouldSatisfy` (Left 1 ==) . left (length . xes @SqlE)
    it "tst2' fail'" $
      ext' <$> tst2' @3 `shouldSatisfy` (Left 1 ==) . left (length . xes @UpdNE)
    it "tst21A good" $
      ext' <$> tst21A @3 `shouldBe` Right ([5, 12, 7, 3, 4, 99, 22], ())
    --    it "tst3rgood" $ right ext tst3rgood `shouldBe` Right [One (unsafeRefined3 123 "123")]
    --    it "tst3rbad" $ hasError @DecodingE tst3rbad

    it "test UpdN failed" $ hasError @UpdNE $ processRetCol @'[UEQ 5] defDec [Left 4]

    it "test Range failed" $ hasError @NoResultSetE $ processRetCol @'[Range 3 5 Upd] defDec [Left 4, Left 5]

    it "test UpdN failed" $ hasError @UpdNE $ ext <$> processRetCol @'[UpdN ( 'OGT ( 'SPos 24) ':||: 'OLT ( 'SPos 14))] defDec [Left 23]

    it "should allow complex UpdN predicate" $
      ext <$> processRetCol @'[UpdN ( 'OGT ( 'SPos 24) ':||: 'OLT ( 'SPos 14))] defDec [Left 8]
        `shouldBe` Right 8

    it "should allow Rev Upd then SelRow" $
      ext <$> processRetCol @'[Rev (SelRow Bool), Upd] defDec [Left 1, trueRS1]
        `shouldBe` Right (True, 1)

    it "should allow SelRow Int then Upd then Rev SelRow Bool and SelRow Bool" $
      ext <$> processRetCol @'[SelRow Int, Rev (SelRow Bool :*: SelRow Bool), Upd] defDec [intRS1 1, Left 2, trueRS1, falseRS1]
        `shouldBe` Right (1, (False, True), 2)

    it "should allow Exact 3 Bools then Rev Range 0 3 Upd" $
      ext <$> processRetCol @'[Rev (Range 0 3 Upd), Exact 3 (SelRow Bool)] defDec [trueRS1, falseRS1, falseRS1, Left 1, Left 2, Left 3]
        `shouldBe` Right ([3, 2, 1], True :| [False, False])

    it "should allow a Bool then Upd then Rev Maybe an Upd " $
      ext <$> processRetCol @'[SelRow Bool, Rev (May (ULT 7)), UGT 24] defDec [trueRS1, Left 25, Left 1]
        `shouldBe` Right (True, Just 1, 25)

    it "should allow a Bool then Upd then Rev Maybe an Upd " $
      ext <$> processRetCol @'[SelRow Bool, Rev (May (ULT 7)), UGT 24] defDec [trueRS1, Left 27, Left 4]
        `shouldBe` Right (True, Just 4, 27)

    it "should allow SelRow SelRow then Rev (Exact 3 Upd)" $
      ext <$> processRetCol @'[Rev (Exact 3 Upd), SelRow Bool, SelRow Bool] defDec ([trueRS1] <> [falseRS1] <> map Left [1 .. 3])
        `shouldBe` Right (3 :| [2, 1], True, False)

    it "should allow combined Sel Bool then Rev Upd" $
      ext <$> processRetCol @'[Rev Upd, SelRow Bool] defDec ([trueRS1] <> [Left 1])
        `shouldBe` Right (1, True)

    it "should allow combined Rev Exact 3 SelRow then 3 Upds" $
      ext <$> processRetCol @'[Rev (Exact 3 Upd), Exact 3 (SelRow Bool)] defDec ([trueRS1, falseRS1, falseRS1] <> map Left [1 .. 3])
        `shouldBe` Right (3 :| [2, 1], True :| [False, False])

    it "should allow Alle and Exact" $
      ext <$> processRetCol @'[Alle (Exact 2 Upd)] defDec (map Left [1 .. 6])
        `shouldBe` Right [1 :| [2], 3 :| [4], 5 :| [6]]

    it "should allow nested Exact then SelRow" $
      ext <$> processRetCol @'[Exact 2 (Exact 3 Upd), SelRow Bool] defDec (map Left [1 .. 6] <> [trueRS1])
        `shouldBe` Right ((1 :| [2, 3]) :| [4 :| [5, 6]], True)

    it "should allow nested Rev Exact 3 Upds then SelRow" $
      ext <$> processRetCol @'[Rev (Exact 3 Upd), SelRow Bool] defDec ([trueRS1] <> map Left [1 .. 3])
        `shouldBe` Right (3 :| [2, 1], True)

    it "should allow nested Exact 2 or SelRow using left only" $
      ext <$> processRetCol @'[Exact 2 (Exact 2 Upd :+: SelRow Bool)] defDec (map Left [1 .. 2] <> replicate 1 trueRS1)
        `shouldBe` Right (Left (1 :| [2]) :| [Right True])

    it "should allow nested Exact 2 or SelRow mixed" $
      ext <$> processRetCol @'[Exact 2 (Exact 3 Upd :+: SelRow Bool)] defDec ([trueRS1] <> map Left [1 .. 3])
        `shouldBe` Right (Right True :| [Left (1 :| [2, 3])])

    it "should allow nested Exact 2 or SelRow right" $
      ext <$> processRetCol @'[Exact 2 (Exact 3 Upd :+: SelRow Bool), SelRow Bool] defDec (map Left [1 .. 3] <> [trueRS1, falseRS1])
        `shouldBe` Right (Left (1 :| [2, 3]) :| [Right True], False)

    it "should allow SelRow Int or SelRow Bool" $
      ext <$> processRetCol @'[SelRow Int :+: SelRow Bool] defDec [intRS1 10]
        `shouldBe` Right (Left 10)

    it "should allow SelRow Int or SelRow Bool" $
      ext <$> processRetCol @'[SelRow Int :+: SelRow Bool] defDec [falseRS1]
        `shouldBe` Right (Right False)

    it "should allow two SelRow Bool or one" $
      ext <$> processRetCol @'[Exact 2 (SelRow Bool) :+: SelRow Bool] defDec [trueRS1, falseRS1]
        `shouldBe` Right (Left (True :| [False]))

    it "should allow two SelRow Bool or one" $
      ext <$> processRetCol @'[Exact 2 (SelRow Bool) :+: SelRow Bool] defDec [trueRS1]
        `shouldBe` Right (Right True)

    it "should Upd then two SelRow Bool then Upd" $
      ext <$> processRetCol @'[Upd, Exact 2 (SelRow Bool), Upd] defDec [Left 7, trueRS1, falseRS1, Left 12]
        `shouldBe` Right (7, True :| [False], 12)

    it "should chooose between the first of three Upd types" $
      ext <$> processRetCol @'[UpdN ( 'OEQ ( 'SPos 7)) :+: UpdN ( 'OEQ ( 'SPos 3)) :+: Upd] defDec (map Left [7])
        `shouldBe` Right (Left (Left 7))

    it "should chooose between the second of three Upd types" $
      ext <$> processRetCol @'[UpdN ( 'OEQ ( 'SPos 7)) :+: UpdN ( 'OEQ ( 'SPos 3)) :+: Upd] defDec (map Left [3])
        `shouldBe` Right (Left (Right 3))

    it "should chooose between the third of three Upd types" $
      ext <$> processRetCol @'[UpdN ( 'OEQ ( 'SPos 7)) :+: UpdN ( 'OEQ ( 'SPos 3)) :+: Upd] defDec (map Left [9])
        `shouldBe` Right (Right 9)

    it "should optionally consume the Upd type" $
      ext <$> processRetCol @'[May (UpdN ( 'OEQ ( 'SPos 7))), Upd] defDec (map Left [9])
        `shouldBe` Right (Nothing, 9)

    it "should optionally consume the Upd type" $
      ext <$> processRetCol @'[May (UpdN ( 'OEQ ( 'SPos 7)))] defDec (map Left [7])
        `shouldBe` Right (Just 7)

    it "should optionally consume the Sel type" $
      ext <$> processRetCol @'[May (UpdN ( 'OEQ ( 'SPos 7))), SelRow Bool] defDec [trueRS1]
        `shouldBe` Right (Nothing, True)

    it "test out RangeT" $
      ext <$> processRetCol @'[Range 4 7 Upd] defDec (map Left [1 .. 6])
        `shouldBe` Right [1, 2, 3, 4, 5, 6]

    it "test out RangeT: more complex" $
      ext <$> processRetCol @'[SelRow Bool, Range 4 7 Upd, SelRow Bool] defDec ([trueRS1] ++ map Left [1 .. 6] ++ [falseRS1])
        `shouldBe` Right (True, [1, 2, 3, 4, 5, 6], False)

    it "test out RangeT: more complex" $
      ext <$> processRetCol @'[Range 1 9 Upd] defDec (map Left [1 .. 9])
        `shouldBe` Right [1, 2, 3, 4, 5, 6, 7, 8, 9]

    it "test out RangeT: more complex" $
      ext <$> processRetCol @'[Range 1 9 Upd] defDec (map Left [1 .. 5])
        `shouldBe` Right [1, 2, 3, 4, 5]

    it "test out Range: more complex" $
      ext <$> processRetCol @'[SelRow Bool, Range 4 7 Upd, SelRow Bool] defDec ([trueRS1] ++ map Left [1 .. 6] ++ [falseRS1])
        `shouldBe` Right (True, [1, 2, 3, 4, 5, 6], False)

    it "test out Range(1)" $ -- why does this fail and not Range 4 7?
      ext <$> processRetCol @'[Range 1 9 Upd] defDec (map Left [1 .. 6])
        `shouldBe` Right [1, 2, 3, 4, 5, 6]

    it "test out Range(2)" $
      ext <$> processRetCol @'[Range 4 7 Upd] defDec (map Left [1 .. 6])
        `shouldBe` Right [1, 2, 3, 4, 5, 6]

    it "test out Range: more complex(2)" $
      ext <$> processRetCol @'[SelRow Bool, Range 4 7 (Upd :*: SelRow Int), SelRow Bool] defDec ([trueRS1] ++ concatMap (\i -> [Left i, intRS1 i]) [1 .. 6] ++ [falseRS1])
        `shouldBe` Right (True, [(1, 1), (2, 2), (3, 3), (4, 4), (5, 5), (6, 6)], False)

    it "test out Range: more complex(2)" $
      ext <$> processRetCol @'[Range 4 7 (Upd :*: SelRow Int)] defDec (concatMap (\i -> [Left i, intRS1 i]) [1 .. 6])
        `shouldBe` Right [(1, 1), (2, 2), (3, 3), (4, 4), (5, 5), (6, 6)]

    it "test out Both(1)" $
      ext <$> processRetCol @'[Upd :*: SelRow Int] defDec [Left 4, intRS1 7]
        `shouldBe` Right (4, 7)

    it "test out Both(2)" $
      ext <$> processRetCol @'[SelRow Bool, Upd :*: SelRow Int, SelRow Bool] defDec ([falseRS1] ++ [Left 4, intRS1 7] ++ [trueRS1])
        `shouldBe` Right (False, (4, 7), True)

    it "test out Rev" $
      ext <$> processRetCol @'[Rev Upd, SelRow Bool] defDec ([trueRS1] <> [Left 3])
        `shouldBe` Right (3, True)

    it "test out Rev for variable number of resultsets(1)" $
      ext <$> processRetCol @'[Rev (May Upd), SelRow Bool] defDec ([trueRS1] <> [Left 3])
        `shouldBe` Right (Just 3, True)

    it "test out Rev for variable number of resultsets(2)" $
      ext <$> processRetCol @'[Rev (May Upd), SelRow Bool] defDec [trueRS1]
        `shouldBe` Right (Nothing, True)

    it "test SelRowCol with 1 value" $
      ext <$> processRetCol @'[SelRowCol Bool] defDec [trueRS1]
        `shouldBe` Right True

    it "SelRowCol with more than one columns" $
      hasError @MoreThanOneColumnE (processRetCol @'[SelRowCol Bool] defDec [rset [[SqlBool False, SqlBool True]]])

    it "SelRowCol with more than one columns" $
      hasError @MoreThanOneColumnE $ processRetCol @'[SelCol Int] defDec [Right ([], [[SqlInt32 123, SqlInt32 456]])]

    it "SelRowCol with more than one row" $
      hasError @ExpectedOneRowE $ processRetCol @'[SelRowCol Int] defDec [Right ([], [[SqlInt32 123], [SqlInt32 456]])]

    it "test SelCol with 1 value" $
      ext <$> processRetCol @'[SelCol Bool] defDec [rset [[SqlBool False], [SqlBool True]]]
        `shouldBe` Right [False, True]

    it "test SelCol with no values" $
      ext <$> processRetCol @'[SelCol Bool] defDec [rset []]
        `shouldBe` Right []

    it "SelCol with more than one columns" $
      hasError @MoreThanOneColumnE (processRetCol @'[SelCol Bool] defDec [rset [[SqlBool False, SqlBool True]]])

    it "test mega reversals 1" $
      ext <$> processRetCol @'[Rev (UEQ 7 :*: SelRow Bool)] defDec [trueRS1, Left 7]
        `shouldBe` Right (7, True)

    it "test mega reversals 2" $
      ext <$> processRetCol @'[Rev (UEQ 7) :*: Rev (SelRow Bool)] defDec [trueRS1, Left 7]
        `shouldBe` Right (7, True)

    it "test mega reversals 3" $
      ext <$> processRetCol @'[Rev (Rev (UEQ 7)) :*: Rev (SelRow Bool)] defDec [Left 7, trueRS1]
        `shouldBe` Right (7, True)

    it "X test mega reversals 4" $
      ext <$> processRetCol @'[Rev (UEQ 7 :*: Rev (SelRow Bool)), Rev (UEQ 1)] defDec [trueRS1, Left 1, Left 7]
        `shouldBe` Right ((7, True), 1)

    it "X test mega reversals 4A" $
      ext <$> processRetCol @'[Rev (UEQ 7 :*: Rev (SelRow Bool))] defDec [trueRS1, Left 7]
        `shouldBe` Right (7, True)

    it "X test or and reversal 4B" $
      ext <$> processRetCol @'[Rev (Exact 2 Upd) :+: SelRow Int] defDec [Left 1, Left 7]
        `shouldBe` Right (Left (7 :| [1]))

    it "test mega reversals 5" $
      ext <$> processRetCol @'[Rev (UEQ 7 :*: SelRow Bool), Rev (UEQ 1)] defDec [Left 1, trueRS1, Left 7]
        `shouldBe` Right ((7, True), 1)

    it "test mega reversals 6" $
      ext <$> processRetCol @'[Rev (UEQ 7 :*: SelRow Bool)] defDec [trueRS1, Left 7]
        `shouldBe` Right (7, True)

    it "test mega reversals 7" $
      ext <$> processRetCol @'[Rev (UEQ 7 :*: SelRow Bool), UEQ 1] defDec [Left 1, trueRS1, Left 7]
        `shouldBe` Right ((7, True), 1)

    it "test crazy reversals 8" $
      ext <$> processRetCol @'[(Rev (SelRow Int) :+: Rev Upd) :*: Rev (Rev (UEQ 7 :*: Rev (SelRow Bool))) :*: Rev Upd] defDec [Left 7, Left 1, trueRS1, Left 0]
        `shouldBe` Right ((Right 0, (7, True)), 1)

    it "test crazy reversals 8" $
      ext <$> processRetCol @'[Exact 2 (SelRow Int) :*: Rev (Range 0 5 Upd) :*: Range 0 5 (SelRow Int)] defDec (map intRS1 [10 .. 14] ++ map Left [1 .. 5])
        `shouldBe` Right ((10 :| [11], [5, 4, 3, 2, 1]), [12, 13, 14])

valid0 :: Rec SingleIn '[SelRow Int]
valid0 = defDec

invalidExact0 :: Rec SingleIn '[Exact 0 Upd]
invalidExact0 = defDec

invalid2a :: Rec SingleIn '[Exact 2 Upd]
invalid2a = defDec

validAlleExact :: Rec SingleIn '[Alle (Exact 2 Upd)]
validAlleExact = defDec

validNestedExactSelRow :: Rec SingleIn '[Exact 2 (Exact 3 Upd), SelRow Bool]
validNestedExactSelRow = defDec

validExact2AndAlleSelRow :: Rec SingleIn '[Exact 2 Upd, Alle (SelRow Bool)]
validExact2AndAlleSelRow = defDec

validUpdOrSelRow :: Rec SingleIn '[Upd :+: SelRow Int]
validUpdOrSelRow = defDec

validUpdAndSelRow :: Rec SingleIn '[Upd, SelRow Int]
validUpdAndSelRow = defDec

valid4a :: Rec SingleIn '[Exact 1 Upd, UGT 24]
valid4a = defDec

valid6 :: Rec SingleIn '[Exact 1 Upd]
valid6 = defDec

rsa :: ResultSet
rsa = Right ([], [[SqlBool True, SqlChar 'x'], [SqlBool False, SqlChar 'y']])

rsb :: Either Int a
rsb = Left 4

rsc :: ResultSet
rsc = Right ([], [[SqlString "xx", SqlBool True, SqlChar 'a']])

tst1 :: Either SE (Rec RState '[Sel (Bool, Char), Upd, Sel R1])
tst1 = processRetCol defDec [rsa, rsb, rsc]

tst2 :: Either SE (Rec RState '[Sel (Bool, Char), Upd, Sel R1])
tst2 = processRetCol defDec [rsa, rsb, rsc]

-- test type level predicate for update: expected @4
tst2' :: forall n. KnownNat n => Either SE (Rec RState '[Sel (Bool, Char), UpdN ( 'OEQ ( 'SPos n)), Sel R1])
tst2' = processRetCol defDec [rsa, rsb, rsc]

-- predicate on length of results and n=0,1,2,3 work but nothing above that
tst21A :: forall n. KnownNat n => Either SE (Rec RState '[Alle (UpdN ( 'OGE ( 'SPos n)))])
tst21A = processRetCol defDec [Left 5, Left 12, Left 7, Left 3, Left 4, Left 99, Left 22]

-- UGE n == UpdN 'OGE n
tst21B :: forall n. KnownNat n => Either SE (Rec RState '[Alle (UGE n)])
tst21B = processRetCol defDec [Left 5, Left 12, Left 7, Left 3, Left 4, Left 99, Left 22]

-- invalid string for sqlchar
tst3 :: Either SE (Rec RState '[Sel (Bool, String), Upd, Sel R1])
tst3 = processRetCol defDec [rsa, rsb, rsc]

{-
tst4r :: (Integral i, Integral j) => i -> j -> Either SE (Rec RState '[Sel (Text, Refined 'OA (4 <..> 10 && Id /= 7) Int, Double)])
tst4r i j = processRetCol defDec [Right ([], [[SqlString "abc", SqlInt32 (fromIntegral i), SqlDouble 1.2], [SqlString "abc", SqlInt32 (fromIntegral j), SqlDouble 1.2]])]

tst4r1 :: (Integral i, Integral j) => i -> j -> Either SE (Rec RState '[Sel (Text, Refined 'OA (Guard (PrintF "oops val=%03d" Id) (Between 4 10 Id && Id /= 7) >> 'True) Int, Double)])
tst4r1 i j = processRetCol defDec [Right ([], [[SqlString "abc", SqlInt32 (fromIntegral i), SqlDouble 1.2], [SqlString "abc", SqlInt32 (fromIntegral j), SqlDouble 1.2]])]
-}
tst3r :: Either SE (Rec RState '[Sel (One String)])
tst3r = processRetCol defDec [Right ([], [[SqlString "123"]])]

{-
-- tst3r1 "1230" checkdigit succeeds
tst3r1 :: String -> Either SE (Rec RState '[Sel (One (LuhnR 'OA 4))])
tst3r1 s = processRetCol defDec [Right ([], [[SqlString s]])]

tst3r2 :: Either SE (Rec RState '[Sel (One (LuhnR 'OA 4))])
tst3r2 = processRetCol defDec [Right ([], [[SqlInt32 111]])]

tst3r3 :: String -> Either SE (Rec RState '[SelRow (One (Refined3 'OA (ReadP Int Id) (Gt 4) (ExitWhen (PrintF "Bad output=%d" Id) (Gt 10) >> ShowP Id) String))])
tst3r3 s = processRetCol defDec [Right ([], [[SqlString s]])]

tst3rgood :: Either SE (Rec RState '[Sel (One (Refined3 'OA (ReadP Int Id) (Gt 4) (ShowP Id) String))])
tst3rgood = processRetCol defDec [Right ([], [[SqlString "123"]])]

tst3rbad :: Either SE (Rec RState '[Sel (One (Refined3 'OA (ReadP Int Id) (Gt 4) (ShowP Id) String))])
tst3rbad = processRetCol defDec [Right ([], [[SqlString "-123"]])]
-}
{-
>tst3rgood
Right {RState {rsIn = SelP Dec<fn>, rsOut = Sel {unSel = [One {unOne = Refined3 123 "123"}]}, rsMeta = [[]]}}
it ::
  Either
    SE
    (Rec
       RState
       '[Sel
           (One
              (Predicate.Refined3.Refined3
                 'Predicate.Util.OA
                 (Predicate.Data.ReadShow.ReadP Int Predicate.Core.Id)
                 (Predicate.Data.Ordering.Gt 4)
                 (Predicate.Data.ReadShow.ShowP Predicate.Core.Id)
                 String))])
>tst3rbad
Left ({|SqlE {sicInstance = "Sel", sicPos = Just 0, sicMessage = "", sicRss = [Right ([],[[SqlString "-123"]])]}|} :| [{|DecodingE method=selImpl | row/col (1,1) | sqlvalues=|},{|DecodingE method=Refined3 Step 2. False Boolean Check(op) | { -123 > 4} |
*** Step 1. Success Initial Conversion(ip) (-123) ***
P ReadP Int -123
|
`- P Id "-123"
*** Step 2. False Boolean Check(op) ***
False -123 > 4
|
+- P Id -123
|
`- P '4
 | sqlvalues=|}])
it ::
  Either
    SE
    (Rec
       RState
       '[Sel
           (One
              (Predicate.Refined3.Refined3
                 'Predicate.Util.OA
                 (Predicate.Data.ReadShow.ReadP Int Predicate.Core.Id)
                 (Predicate.Data.Ordering.Gt 4)
                 (Predicate.Data.ReadShow.ShowP Predicate.Core.Id)
                 String))])
-}

tst4 :: Either SE (Rec RState '[Sel (Bool, Char)])
tst4 = processRetCol defDec [rsa]

gtest1, gtest2, gtest2' :: Either SE (Rec RState '[Upd, Sel (Int, Bool)])
gtest1 = processRetCol defDec [Left 123]
gtest2 = processRetCol defDec [Left 123, Right ([], [[SqlInt32 11, SqlBool True], [SqlInt32 22, SqlBool False]])]
gtest2' = processRetCol defDec [Left 123, Right ([], [[SqlInt32 11, SqlBool True], [SqlInt32 22, SqlBool False]])]

gtest3 :: Either SE (Rec RState '[Upd, Sel (Text, Bool)])
gtest3 = processRetCol defDec [Left 123, Right ([], [[SqlString "afield", SqlBool True], [SqlString "zzz", SqlBool False]])]

gtest4 :: Either SE (Rec RState '[Alle Upd])
gtest4 = processRetCol defDec [Left 123, Left 999, Left (-8)]

gtest5, gtest5A, gtest5B, gtest5C, gtest5D :: Either SE (Rec RState '[Upd :+: SelRow Int])
gtest5 = processRetCol defDec [Left 123]
gtest5A = processRetCol defDec [Right ([], [[SqlInt32 123]])]
gtest5B = processRetCol defDec [Right ([], [[SqlInt32 123], [SqlInt32 456]])] -- fails cos too many rows
gtest5C = processRetCol defDec [Right ([], [[SqlInt32 123, SqlInt32 456]])] -- fails cos not and update and fails cos too many columns for SelRow Int
gtest5D = processRetCol defDec [Right ([], [])] -- fails cos not enough rows

gtest6, gtest6A, gtest6B, gtest6C, gtest6D :: Either SE (Rec RState '[SelRow Int :+: Upd])
gtest6 = processRetCol defDec [Right ([], [[SqlInt32 123]])]
gtest6A = processRetCol defDec [Left 4]
gtest6B = processRetCol defDec [Right ([], [[SqlInt32 123], [SqlInt32 456]])] -- fails cos too many rows
gtest6C = processRetCol defDec [Right ([], [[SqlInt32 123, SqlInt32 456]])] -- fails cos too many values
gtest6D = processRetCol defDec [Right ([], [])] -- fails cos not enough rows

x1 :: SingleIn Upd
x1 = defDec

x1A :: SingleIn Upd
x1A = defDec

x1B :: SingleIn (Alle Upd)
x1B = defDec

x1C :: SingleIn (Upd :+: SelRow Int)
x1C = defDec

x1D :: SingleIn (SelRow Int :+: Upd)
x1D = defDec

x2 :: SingleIn (Sel (Int, Bool))
x2 = defDec

x2' :: SingleIn (Sel (Int, Bool))
x2' = defDec

x3' :: SingleIn (Sel (Text, Bool))
x3' = defDec

validx1 :: Rec SingleIn '[Upd :+: SelRow Int]
validx1 = defDec

validx2 :: Rec SingleIn '[SelRow Int]
validx2 = defDec

validx3 :: Rec SingleIn '[Upd, Alle Upd]
validx3 = defDec

validx4 :: Rec SingleIn '[Upd, Upd, Sel (Double, Int)]
validx4 = defDec

s0 :: Sql db '[Int] '[Upd]
s0 = Sql "zzz" (defEnc :& RNil) (x1 :& RNil) "s0"

s1 :: Sql db '[Int, Double] '[Upd, Sel (Int, Bool)]
s1 = Sql "abc" (defEnc :& defEnc :& RNil) (x1A :& x2' :& RNil) "s1"

-- same as s1 but dont need to specify each defEnc
s1a :: Sql db '[Int, Double] '[Upd, Sel (Int, Bool)]
s1a = Sql "abc" defEnc (x1A :& x2' :& RNil) "s1"

-- Alle is not last but gets detected when used with runSql
s2 :: Sql db '[] '[Alle Upd, Upd :+: SelRow Int]
s2 = Sql "def" RNil (x1B :& x1C :& RNil) "s2"

s3 :: Sql db '[] '[Upd :+: SelRow Int, Alle Upd]
s3 = Sql "def" RNil (x1C :& x1B :& RNil) "s3"

s4 :: Sql db '[Bool, Text] '[Upd :+: SelRow Int, SelRow Char, Alle (Sel Int)]
s4 =
  Sql
    "def"
    (defEnc :& defEnc :& RNil)
    ( UpdP :+: SelRowP defDec
        :& SelRowP defDec
        :& AlleP defDec
        :& RNil
    )
    "s4"

gtest0 :: Rec RState '[SelRow (F '["aa" ::: Int])]
gtest0 = RState defDec (SelRow ((#aa =: 123) :& RNil) []) :& RNil

gtest00 :: Rec RState '[SelRow (F '["aa" ::: Int, "bb" ::: Bool])]
gtest00 = RState defDec (SelRow ((#aa =: 123) :& (#bb =: True) :& RNil) []) :& RNil

gtest000 :: Rec RState '[Sel (F '["aa" ::: Int, "bb" ::: Bool])]
gtest000 = RState defDec (Sel [(#aa =: 123) :& (#bb =: True) :& RNil] []) :& RNil

gtest0000 :: Rec RState '[Sel (F '["aa" ::: Int, "bb" ::: Bool])]
gtest0000 = RState defDec (Sel [(#aa =: 123) :& (#bb =: True) :& RNil, (#aa =: 321) :& (#bb =: False) :& RNil] []) :& RNil

ys' :: Rec SingleIn '[Upd, SelRaw, Alle (SelRow Int)]
ys' = UpdP :& SelRawP :& AlleP (SelRowP (defDec @(Dec Int))) :& RNil

-- should always fail cos has an alle but not at the end
zs' :: Rec SingleIn '[Upd, SelRaw, Alle (SelRow Int), Alle Upd]
zs' = UpdP :& SelRawP :& AlleP (SelRowP (defDec @(Dec Int))) :& AlleP UpdP :& RNil

testColDesc :: String -> SqlColDesc
testColDesc s = SqlColDesc s (SqlUnknownT "for testing sql.hs") (Just 5) (Just 7) (Just 11) (Just True)

tt1' :: [ResultSet]
tt1' = [Left 3, Right ([], [[SqlChar 'c']]), Right ([], [[SqlInt32 12], [SqlInt32 456], [SqlInt32 999]])]

tt2' :: [ResultSet]
tt2' = [Left 3, Right ([testColDesc "a1"], [[SqlChar 'c']]), Right ([testColDesc "b1"], [[SqlInt32 12], [SqlInt32 456], [SqlInt32 999]])]

t11 ::
  ( ( SingleOut (Alle (Upd :+: Sel Int)) ~ [Either Int [Int]]
    , SingleOut (SelRow Bool :+: Sel Int) ~ Either Bool [Int]
    , SingleOut Upd ~ Int
    , SingleOut (Sel (Bool, String)) ~ [(Bool, String)]
    , SingleOut (SelRow (Bool, String)) ~ (Bool, String)
    , SingleOuts '[Upd, Upd, Sel (Bool, String)] ~ (Int, Int, [(Bool, String)])
    , SingleOut (Sel (MakeF (Bool, String))) ~ [Rec ElField '["c1" ::: Bool, "c2" ::: String]]
    , SingleOut (Sel (MakeF' '[Bool, String])) ~ [Rec ElField '["c1" ::: Bool, "c2" ::: String]]
    , SingleOut (Sel (F '["abool" ::: Bool, "astring" ::: String])) ~ [Rec ElField '["abool" ::: Bool, "astring" ::: String]]
    , SingleOut (Alle (Upd :+: SelRaw)) ~ [Either Int [[SqlValue]]]
    ) =>
    ()
  ) ->
  ()
t11 x = x

t12 :: forall db. Sql db '[Int] '[UpdN ( 'OEQ ( 'SPos 0)), Sel Int, SelRow Bool]
t12 =
  let _a :: Text
      _b :: Rec Enc '[Int]
      _c :: Rec SingleIn '[UpdN ( 'OEQ ( 'SPos 0)), Sel Int, SelRow Bool]
      _d :: Text
      z :: Sql db '[Int] '[UpdN ( 'OEQ ( 'SPos 0)), Sel Int, SelRow Bool]
      z@(Sql _a _b _c _d) = sqlCombine (mkSql @'[U0] @'[] "[sqlA]" "select 1") (mkSql @'[Sel Int, SelRow Bool] @'[Int] "[sqlB]" "select 2")
   in z
