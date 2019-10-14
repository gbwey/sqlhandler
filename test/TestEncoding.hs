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
import EasyTest
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
  , scope "encoding.ex11" $ expectEq (unEnc (divided (defEnc :: Enc Int) (defEnc :: Enc Bool)) (24,True)) [SqlInt32 24,SqlInt32 1]
  , scope "encoding.ex12" $ expectEq (unEnc (encList' encBool) [True,False]) [SqlInt32 1,SqlInt32 0]
  , scope "encoding.ex13" $ expectEq (unEnc (encList' encBoolMS) [True,False]) [SqlChar '\SOH',SqlChar '\NUL']
  , scope "encoding.ex14" $ expectEq (unEnc defEnc (unsafeRefined @'True @String "abc")) [SqlString "abc"]
  , scope "encoding.ex15" $ expectEq (unEnc defEnc (unsafeRefined3 @Id @'True @Id @String "abc" "abc")) [SqlString "abc"]
  , scope "encoding.ex16" $ expectEq (unEnc defEnc (unsafeRefined3 @(ReadBase Int 16) @(Gt 10) @(ShowBase 16) @String 254 "fe")) [SqlString "fe"]
  , scope "encoding.ex17" $ expectEq (unEnc @(MakeR3 DateN) defEnc (unsafeRefined3 (fromGregorian 2001 12 3) "2001-12-03")) [SqlString "2001-12-03"]
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




