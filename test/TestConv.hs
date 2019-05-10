{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module TestConv where
import Control.Monad
import EasyTest
import Conv
import Database.HDBC (SqlValue(..))
import Data.Text (Text)
import Test.Hspec

spec :: SpecWith ()
spec =
  describe "Type Tests" $
    it "should allow combined Some and Alle with no Alle data" $
      conv @Bool [SqlBool True] `shouldBe` Right True

suite :: Test ()
suite = tests
  [ scope "ceq0.ok" $ void $ expectEq (conv @Bool [SqlBool True]) (Right True)
  , scope "ceq1.fail" $ void $ expectLeft (conv @Bool [SqlInt32 3])
  , scope "ceq2.ok" $ void $ expectEq (conv @Text [SqlString "afield1"]) (Right @_ @Text "afield1")
  , scope "ceq3.ok" $ void $ expectEq (conv @String [SqlByteString "afield2"]) (Right @_ @String "afield2")
  , scope "ceq4.ok" $ void $ expectEq (conv @(String,Int,Bool) [SqlString "afield3",SqlDouble 1.0,SqlInt32 1]) (Right @_ @(String,Int,Bool) ("afield3",1,True))
  ]
