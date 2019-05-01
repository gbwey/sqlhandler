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
  , scope "ceq2.ok" $ void $ expectEq (conv @Text [SqlString "dude1"]) (Right @_ @Text "dude1")
  , scope "ceq3.ok" $ void $ expectEq (conv @String [SqlByteString "dude2"]) (Right @_ @String "dude2")
  , scope "ceq4.ok" $ void $ expectEq (conv @(String,Int,Bool) [SqlString "dude3",SqlDouble 1.0,SqlInt32 1]) (Right @_ @(String,Int,Bool) ("dude3",1,True))
  ]
