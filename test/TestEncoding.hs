{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS -Wall #-}
module TestEncoding where
import Data.ByteString (ByteString)
import Data.Text (Text)
import Database.HDBC (SqlValue(..))
import Test.Tasty
import Test.Tasty.HUnit
import Encoding
import Data.Functor.Contravariant.Divisible
import qualified Generics.SOP as GS
import qualified GHC.Generics as G
import Test.Hspec
import Sql
import Predicate
import Refined3
import Refined3Helper
import Refined
import Data.Time

spec :: SpecWith ()
spec =
  describe "Type Tests" $
    it "should allow combined Some and Alle with no Alle data" $
      unEnc defEnc (1::Int) `shouldBe` [SqlInt32 1]

suite :: IO ()
suite = defaultMain $ testGroup "TestEncoding"
  [ testCase "encoding.ex1" $ (@?=) (unEnc defEnc (1::Int)) [SqlInt32 1]
  , testCase "encoding.ex2" $ (@?=) (unEnc defEnc (True,'c')) [SqlInt32 1,SqlChar 'c']
  , testCase "encoding.ex3" $ (@?=) (unEnc defEnc ("\nabc " :: ByteString)) [SqlByteString "\nabc "]
  , testCase "encoding.ex4" $ (@?=) (unEnc defEnc ("defEnc " :: String)) [SqlString "defEnc "]
  , testCase "encoding.ex5" $ (@?=) (unEnc defEnc ("ghi" :: Text)) [SqlString "ghi"]
  , testCase "encoding.ex6" $ (@?=) (unEnc encHMS (10,12,13)) [SqlString "10:12:13"]
  , testCase "encoding.ex7" $ (@?=) (encodeVals (E2 (defEnc @(Enc Int)) (defEnc @(Enc (Bool,String)))) (I2 1 (True,"xxx"))) [SqlInt32 1,SqlInt32 1,SqlString "xxx"]
  , testCase "encoding.ex8" $ (@?=) (encodeVals (E1 (defEnc @(Enc (Int, (Bool,String))))) (I1 (1,(True,"xxx")))) [SqlInt32 1,SqlInt32 1,SqlString "xxx"]
--  , testCase "encoding.ex9" $ (@?=) (unEnc (encLens _1 (defEnc :: Enc Bool)) (True,"1",'x')) [SqlInt32 1]
--  , testCase "encoding.ex10" $ (@?=) (unEnc (encLens _1 (Enc $ \i -> [SqlInt32 123])) (True,"1",'x')) [SqlInt32 123]
  , testCase "encoding.ex11" $ (@?=) (unEnc (divided (defEnc :: Enc Int) (defEnc :: Enc Bool)) (24,True)) [SqlInt32 24,SqlInt32 1]
  , testCase "encoding.ex12" $ (@?=) (unEnc (encList' encBool) [True,False]) [SqlInt32 1,SqlInt32 0]
  , testCase "encoding.ex13" $ (@?=) (unEnc (encList' encBoolMS) [True,False]) [SqlChar '\SOH',SqlChar '\NUL']
  , testCase "encoding.ex14" $ (@?=) (unEnc defEnc (unsafeRefined @'True @String "abc")) [SqlString "abc"]
  , testCase "encoding.ex15" $ (@?=) (unEnc defEnc (unsafeRefined3 @Id @'True @Id @String "abc" "abc")) [SqlString "abc"]
  , testCase "encoding.ex16" $ (@?=) (unEnc defEnc (unsafeRefined3 @(ReadBase Int 16) @(Gt 10) @(ShowBase 16) @String 254 "fe")) [SqlString "fe"]
  , testCase "encoding.ex17" $ (@?=) (unEnc @(MakeR3 DateN) defEnc (unsafeRefined3 (fromGregorian 2001 12 3) "2001-12-03")) [SqlString "2001-12-03"]
  ]


data T6 = T6 { t66 :: Char, t666 :: Double } deriving (G.Generic, Show)
instance GS.Generic T6
instance GS.HasDatatypeInfo T6
instance DefEnc (Enc T6)

{-
>GO.gfoldMap @DefE (\s -> unEnc defE s) (T6 'x' 123.45)
[SqlChar 'x',SqlDouble 123.45]
it :: [SqlValue]

>unEnc defE (T6 'x' 1233.5)
[SqlChar 'x',SqlDouble 1233.5]
it :: [SqlValue]
-}




