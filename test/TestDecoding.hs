{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS -Wall #-}
module TestDecoding where
import EasyTest
import Decoding
import Database.HDBC (SqlValue(..))
import Data.Functor
import Test.Hspec
import Data.Vinyl (Rec(..))
import qualified Data.Vinyl as V
import VinylUtils
import RegexHelper (IP(..))

spec :: SpecWith ()
spec =
  describe "Type Tests" $
    it "should allow combined Some and Alle with no Alle data" $
      unDec (defDec :: Dec Int) [SqlInt32 12] `shouldBe` Right (12,[])

suite :: Test ()
suite = tests
  [ scope "decoding.ex1" $ expectEq (unDec (defDec :: Dec Int) [SqlInt32 12]) (Right (12,[]))
  , scope "decoding.ex2" $ expectEq (unDec defDec [SqlInt32 12]) (Right (12::Int,[]))
  , scope "decoding.ex3" $ expectEq (unDec defDec [SqlInteger 12,SqlString "x"]) (Right (12::Int,[SqlString "x"]))
  , scope "decoding.ex4" $ expectEq (unDec defDec [SqlDouble 12.0000]) (Right (12::Double,[]))
  , scope "decoding.ex5" $ expectEq (unDec defDec [SqlDouble 12.33333]) (Right (12.33333::Double,[]))
  , scope "decoding.ex6" $ expectEq (unDec @Float defDec [SqlDouble 12.33333]) (Right (12.33333,[]))
  , scope "decoding.ex7" $ expectEq (unDec defDec [SqlInt32 12, SqlBool True, SqlChar 'c']) (Right ((12::Int,True,'c'),[]))
  , scope "decoding.ex8" $ expectEq (unDec defDec [SqlChar '\0']) (Right (False,[]))
  , scope "decoding.ex9" $ expectEq (unDec defDec [SqlChar '\1']) (Right (True,[]))
  , scope "decoding.ex10" $ void $ expectLeft (unDec (defDec :: Dec (DecN 3 Int)) [SqlInt32 1,SqlInt32 4])
  , scope "decoding.ex11" $ expectEq (unDec (defDec :: Dec (DecN 3 Int)) [SqlInt32 1,SqlInt32 4,SqlInt32 555]) (Right (DecN [1,4,555],[]))
  , scope "decoding.ex12" $ expectEq (unDec (defDec :: Dec (DecN 3 Int)) [SqlInt32 1,SqlInt32 4,SqlInt32 555,SqlInt32 12]) (Right (DecN [1,4,555],[SqlInt32 12]))
  , scope "decoding.ex13" $ expectEq (unDec defDec [SqlString "aa",SqlBool True,SqlChar 'x']) (Right (S1 "aa" True 'x', []))
  , scope "decoding.ex14" $ expectEq (unDec (defDec :: Dec (DecAlle Int)) [SqlInteger 1,SqlInt32 4,SqlInteger 555,SqlInt32 12]) (Right (DecAlle [1,4,555,12],[]))
  , scope "decoding.ex15" $ expectEq (unDec (defDec :: Dec (V.ElField ("abc" V.::: Int))) [SqlInteger 123,SqlString "x"]) (Right (V.Field @"abc" 123,[SqlString "x"]))
  , scope "decoding.ex16" $ expectEq (unDec (defDec :: Dec (F '["abc" V.::: Int, "def" V.::: String])) [SqlInteger 123,SqlString "x"]) (Right (V.Field @"abc" 123 :& V.Field @"def" "x" :& RNil,[]))
  ]

data S1 = S1 { s1 :: !String, s2 :: !Bool, s3 :: !Char } deriving (Show,Eq)
instance DefDec (Dec S1) where
  defDec = S1 <$> defDec <*> defDec <*> defDec

-- needs regexhelper
decIp :: Dec (IP Int)
decIp = defDec >>= \s -> Dec $ \ss ->
                  case reads @(IP Int) s of
                          [] -> failDE "decIp" "failed to process" [SqlString s]
                          (a,_):_ -> Right (a,ss) -- ignores extra junk


