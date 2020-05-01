{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
module TestConv where
import Test.Tasty
import Test.Tasty.HUnit
import HSql.Core.Conv
import Database.HDBC (SqlValue(..))
import Data.Text (Text)
import Test.Hspec

spec :: SpecWith ()
spec =
  describe "Type Tests" $
    it "should allow combined Some and Alle with no Alle data" $
      conv @Bool [SqlBool True] `shouldBe` Right True

suite :: TestTree
suite = testGroup "TestConv"

  [ testCase "ceq0.ok" $ (@?=) (conv @Bool [SqlBool True]) (Right True)
  , testCase "ceq1.fail" $ expectLeft (conv @Bool [SqlInt32 3])
  , testCase "ceq2.ok" $ (@?=) (conv @Text [SqlString "afield1"]) (Right @_ @Text "afield1")
  , testCase "ceq3.ok" $ (@?=) (conv @String [SqlByteString "afield2"]) (Right @_ @String "afield2")
  , testCase "ceq4.ok" $ (@?=) (conv @(String,Int,Bool) [SqlString "afield3",SqlDouble 1.0,SqlInt32 1]) (Right @_ @(String,Int,Bool) ("afield3",1,True))
  ]

expectLeft :: Show b => Either a b -> IO ()
expectLeft = \case
  Left _ -> pure ()
  Right e -> assertFailure $ "expected Left but found Right " ++ show e
