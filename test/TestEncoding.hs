{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall #-}
module TestEncoding where
import Data.ByteString (ByteString)
import Data.Text (Text)
import Database.HDBC (SqlValue(..))
import EasyTest
import Encoding
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Test.Hspec
import Control.Lens
import Sql

spec :: SpecWith ()
spec =
  describe "Type Tests" $
    it "should allow combined Some and Alle with no Alle data" $
      unEnc defEnc (1::Int) `shouldBe` [SqlInt32 1]

suite :: Test ()
suite = tests
  [ scope "encoding.ex1" $ expectEq (unEnc defEnc (1::Int)) [SqlInt32 1]
  , scope "encoding.ex2" $ expectEq (unEnc defEnc (True,'c')) [SqlInt32 1,SqlChar 'c']
  , scope "encoding.ex3" $ expectEq (unEnc defEnc ("\nabc " :: ByteString)) [SqlByteString "\nabc "]
  , scope "encoding.ex4" $ expectEq (unEnc defEnc ("defEnc " :: String)) [SqlString "defEnc "]
  , scope "encoding.ex5" $ expectEq (unEnc defEnc ("ghi" :: Text)) [SqlString "ghi"]
  , scope "encoding.ex6" $ expectEq (unEnc encHMS (10,12,13)) [SqlString "10:12:13"]
  , scope "encoding.ex7" $ expectEq (encodeVals (E2 (defEnc @(Enc Int)) (defEnc @(Enc (Bool,String)))) (I2 1 (True,"xxx"))) [SqlInt32 1,SqlInt32 1,SqlString "xxx"]
  , scope "encoding.ex8" $ expectEq (encodeVals (E1 (defEnc @(Enc (Int, (Bool,String))))) (I1 (1,(True,"xxx")))) [SqlInt32 1,SqlInt32 1,SqlString "xxx"]
--  , scope "encoding.ex9" $ expectEq (unEnc (encLens _1 (defEnc :: Enc Bool)) (True,"1",'x')) [SqlInt32 1]
--  , scope "encoding.ex10" $ expectEq (unEnc (encLens _1 (Enc $ \i -> [SqlInt32 123])) (True,"1",'x')) [SqlInt32 123]
  , scope "encoding.ex10" $ expectEq (unEnc (divided (defEnc :: Enc Int) (defEnc :: Enc Bool)) (24,True)) [SqlInt32 24,SqlInt32 1]
  , scope "encoding.ex10" $ expectEq (unEnc (encList' encBool) [True,False]) [SqlInt32 1,SqlInt32 0]
  , scope "encoding.ex10" $ expectEq (unEnc (encList' encBoolMS) [True,False]) [SqlChar '\SOH',SqlChar '\NUL']
  ]
