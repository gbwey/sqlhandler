{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS -Wall #-}
module TestDecoder where
import Test.Tasty
import Test.Tasty.HUnit
import HSql.Core.Decoder
import Database.HDBC (SqlValue(..))
import Data.Functor
import Test.Hspec
import Data.Vinyl (Rec(..))
import qualified Data.Vinyl as V
import HSql.Core.VinylUtils
import Predicate
import qualified Predicate.Refined2 as R2
import qualified Predicate.Examples.Refined2 as R2
import qualified Predicate.Refined3 as R3
import qualified Predicate.Examples.Refined3 as R3
import HSql.Core.ErrorHandler

spec :: SpecWith ()
spec =
  describe "Type Tests" $
    it "should allow combined Some and Alle with no Alle data" $
      unDec (defDec :: Dec Int) [SqlInt32 12] `shouldBe` Right (12,[])

suite :: TestTree
suite =
  let s = "TestDecoder"
  in testGroup s (orderTests s allTests)

orderTests :: String -> [Assertion] -> [TestTree]
orderTests s = zipWith (\i t -> testCase (s <> show i) t) [1::Int ..]

allTests :: [IO ()]
allTests =
  [ (@?=) (unDec (defDec :: Dec Int) [SqlInt32 12]) (Right (12,[]))
  , (@?=) (unDec defDec [SqlInt32 12]) (Right (12::Int,[]))
  , (@?=) (unDec defDec [SqlInteger 12,SqlString "x"]) (Right (12::Int,[SqlString "x"]))
  , (@?=) (unDec defDec [SqlDouble 12.0000]) (Right (12::Double,[]))
  , (@?=) (unDec defDec [SqlDouble 12.33333]) (Right (12.33333::Double,[]))
  , (@?=) (unDec @Float defDec [SqlDouble 12.33333]) (Right (12.33333,[]))
  , (@?=) (unDec defDec [SqlInt32 12, SqlBool True, SqlChar 'c']) (Right ((12::Int,True,'c'),[]))
  , (@?=) (unDec defDec [SqlChar '\0']) (Right (False,[]))
  , (@?=) (unDec defDec [SqlChar '\1']) (Right (True,[]))
  , void $ expectLeft (unDec (defDec :: Dec (DecN 3 Int)) [SqlInt32 1,SqlInt32 4])
  , (@?=) (unDec (defDec :: Dec (DecN 3 Int)) [SqlInt32 1,SqlInt32 4,SqlInt32 555]) (Right (DecN [1,4,555],[]))
  , (@?=) (unDec (defDec :: Dec (DecN 3 Int)) [SqlInt32 1,SqlInt32 4,SqlInt32 555,SqlInt32 12]) (Right (DecN [1,4,555],[SqlInt32 12]))
  , (@?=) (unDec defDec [SqlString "aa",SqlBool True,SqlChar 'x']) (Right (S1 "aa" True 'x', []))
  , (@?=) (unDec (defDec :: Dec (DecAlle Int)) [SqlInteger 1,SqlInt32 4,SqlInteger 555,SqlInt32 12]) (Right (DecAlle [1,4,555,12],[]))
  , (@?=) (unDec (defDec :: Dec (V.ElField ("abc" V.::: Int))) [SqlInteger 123,SqlString "x"]) (Right (V.Field @"abc" 123,[SqlString "x"]))
  , (@?=) (unDec (defDec :: Dec (F '["abc" V.::: Int, "def" V.::: String])) [SqlInteger 123,SqlString "x"]) (Right (V.Field @"abc" 123 :& V.Field @"def" "x" :& RNil,[]))
  , expectD (Right (R3.unsafeRefined3 [127,1,0,199] "127.001.000.199" ,[])) (unDec (defDec :: Dec (R3.Refined3 'OZ (Map (ReadP Int Id) (Resplit "\\." Id)) (Guard "length" (Len == 4) >> Guard "octet 0-255" (All (Between 0 255 Id) Id) >> 'True) (PrintL 4 "%03d.%03d.%03d.%03d" Id) String)) [SqlString "127.1.0.199"])
  , expectD (Left "Refined3 Step 2. Failed Boolean Check(op) | octet 0-255") (unDec (defDec :: Dec (R3.Refined3 'OZ (Map (ReadP Int Id) (Resplit "\\." Id)) (Guard "length" (Len == 4) >> Guard "octet 0-255" (All (Between 0 255 Id) Id) >> 'True) (PrintL 4 "%03d.%03d.%03d.%03d" Id) String)) [SqlString "127.1.0.499"])
  , expectD (Left "Refined3 Step 2. Failed Boolean Check(op) | octet 3 out of range 0-255 found 499") (unDec (defDec :: Dec (R3.MakeR3 (R3.Ip4 'OZ))) [SqlString "127.1.0.499"])
  , expectD (Left "Refined3 Step 2. Failed Boolean Check(op) | Guards:invalid length(5) expected 4") (unDec (defDec :: Dec (R3.MakeR3 (R3.Ip4 'OZ))) [SqlString "127.1.0.4.5"])
  , expectD (Right (R3.unsafeRefined3 [127,1,0,4] "127.001.000.004", [])) (unDec (defDec :: Dec (R3.MakeR3 (R3.Ip4 'OZ))) [SqlString "127.1.0.4"])
  , expectD (Left "Refined3 Step 2. Failed Boolean Check(op) | octet 3 out of range 0-255 found 400") (unDec (defDec :: Dec (R3.MakeR3 (R3.Ip4 'OZ))) [SqlString "127.1.0.400"])
  , expectD (Right (R3.unsafeRefined3 [123,45,6789] "123-45-6789", [])) (unDec (defDec :: Dec (R3.MakeR3 (R3.Ssn 'OZ))) [SqlString "123-45-6789"])
  , expectD (Left "Refined3 Step 2. False Boolean Check(op) | {Bool(1) [number for group 1 invalid: found 0] (1 <= 0)}") (unDec (defDec :: Dec (R3.MakeR3 (R3.Ssn 'OL))) [SqlString "123-00-6789"])
  , expectD (Left "Refined3 Step 2. False Boolean Check(op) | {Bool(0) [number for group 0 invalid: found 666] (True && False | (666 /= 666))}") (unDec (defDec :: Dec (R3.MakeR3 (R3.Ssn 'OL))) [SqlString "666-01-6789"])
  , expectD (Right (unsafeRefined 8, [])) (unDec (defDec :: Dec (Refined 'OL (Between 4 10 Id && Id /= 7) Int)) [SqlInt32 8])
  , expectD (Left "Refined FalseP(False && True | (4 <= 2))") (unDec (defDec :: Dec (Refined 'OL (Between 4 10 Id && Id /= 7) Int)) [SqlInt32 2])

  , expectD (Right (R2.unsafeRefined2 [127,1,0,199] "127.1.0.199" ,[])) (unDec (defDec :: Dec (R2.Refined2 'OZ (Map (ReadP Int Id) (Resplit "\\." Id)) (Guard "length" (Len == 4) >> Guard "octet 0-255" (All (0 <..> 255) Id) >> 'True)  String)) [SqlString "127.1.0.199"])
  , expectD (Left "Refined2 Step 2. Failed Boolean Check(op) | octet 0-255") (unDec (defDec :: Dec (R2.Refined2 'OZ (Map (ReadP Int Id) (Resplit "\\." Id)) (Guard "length" (Len == 4) >> Guard "octet 0-255" (All (0 <..> 255) Id) >> 'True) String)) [SqlString "127.1.0.499"])
  , expectD (Left "Refined2 Step 2. Failed Boolean Check(op) | octet 3 out of range 0-255 found 499") (unDec (defDec :: Dec (R2.MakeR2 (R2.Ip4 'OZ))) [SqlString "127.1.0.499"])
  , expectD (Left "Refined2 Step 2. Failed Boolean Check(op) | Guards:invalid length(5) expected 4") (unDec (defDec :: Dec (R2.MakeR2 (R2.Ip4 'OZ))) [SqlString "127.1.0.4.5"])

  ]

expectLeft :: Show b => Either a b -> IO ()
expectLeft = \case
  Left _ -> pure ()
  Right e -> assertFailure $ "expected Left but found Right " ++ show e

data S1 = S1 { s1 :: !String, s2 :: !Bool, s3 :: !Char } deriving (Show,Eq)
instance DefDec (Dec S1) where
  defDec = S1 <$> defDec <*> defDec <*> defDec
expectD :: (HasCallStack, Eq r,Show r)
  => Either String r
  -> Either DE r
  -> IO ()
expectD lhs rhs = do
  let rr = case rhs of
            Right r -> Right r
            Left es -> case snd $ getDecErrors es of
                         [] -> error "znork! missing DecodingE"
                         [s] -> Left $ _deMethod s
                         o -> error $ "expected only one DecodingE o=" ++ show o
  rr @?= lhs
