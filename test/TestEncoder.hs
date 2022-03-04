{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module TestEncoder where

import Data.ByteString (ByteString)
import Data.Functor.Contravariant.Divisible
import Data.Text (Text)
import Database.HDBC (SqlValue (..))
import qualified GHC.Generics as G
import qualified Generics.SOP as GS
import HSql.Core.Encoder
import HSql.Core.VinylUtils
import Test.Tasty
import Test.Tasty.HUnit

{-
import Predicate
import qualified Predicate.Examples.Refined2 as R2
import qualified Predicate.Examples.Refined3 as R3
import qualified Predicate.Refined2 as R2
import qualified Predicate.Refined3 as R3
-}
import Test.Hspec

spec :: SpecWith ()
spec =
  describe "Type Tests" $
    it "should allow combined Exact and Alle with no Alle data" $
      unEnc defEnc (1 :: Int) `shouldBe` [SqlInt32 1]

suite :: TestTree
suite =
  let s :: String
      s = "TestEncoder"
   in testGroup s (orderTests s allTests)

orderTests :: String -> [Assertion] -> [TestTree]
orderTests s = zipWith (\i t -> testCase (s <> show i) t) [1 :: Int ..]

allTests :: [IO ()]
allTests =
  [ (@?=) (unEnc defEnc (1 :: Int)) [SqlInt32 1]
  , (@?=) (unEnc defEnc (True, 'c')) [SqlInt32 1, SqlChar 'c']
  , (@?=) (unEnc defEnc ("\nabc " :: ByteString)) [SqlByteString "\nabc "]
  , (@?=) (unEnc defEnc ("defEnc " :: String)) [SqlString "defEnc "]
  , (@?=) (unEnc defEnc ("ghi" :: Text)) [SqlString "ghi"]
  , (@?=) (unEnc encHMS (10, 12, 13)) [SqlString "10:12:13"]
  , (@?=) (encodeVals (E2 (defEnc @(Enc Int)) (defEnc @(Enc (Bool, String)))) (I2 1 (True, "xxx"))) [SqlInt32 1, SqlInt32 1, SqlString "xxx"]
  , (@?=) (encodeVals (E1 (defEnc @(Enc (Int, (Bool, String))))) (I1 (1, (True, "xxx")))) [SqlInt32 1, SqlInt32 1, SqlString "xxx"]
  , (@?=) (unEnc (divided (defEnc :: Enc Int) (defEnc :: Enc Bool)) (24, True)) [SqlInt32 24, SqlInt32 1]
  , (@?=) (unEnc (encList' encBool) [True, False]) [SqlInt32 1, SqlInt32 0]
  , (@?=) (unEnc (encList' encBoolMS) [True, False]) [SqlChar '\SOH', SqlChar '\NUL']
  {-
    , (@?=) (unEnc defEnc (unsafeRefined @ 'OZ @ 'True @String "abc")) [SqlString "abc"]
    , (@?=) (unEnc defEnc (R2.unsafeRefined2 @ 'OZ @Id @ 'True @String "abc" "abc")) [SqlString "abc"]
    , (@?=) (unEnc defEnc (R2.unsafeRefined2 @ 'OZ @(ReadBase Int 16) @(Gt 10) @String 254 "fe")) [SqlString "fe"]
    , (@?=) (unEnc @(R2.MakeR2 (R2.DateN 'OZ)) defEnc (R2.unsafeRefined2 (fromGregorian 2001 12 3) "2001-12-03")) [SqlString "2001-12-03"]
    , (@?=) (unEnc defEnc (R3.unsafeRefined3 @ 'OZ @Id @ 'True @Id @String "abc" "abc")) [SqlString "abc"]
    , (@?=) (unEnc defEnc (R3.unsafeRefined3 @ 'OZ @(ReadBase Int 16) @(Gt 10) @(ShowBase 16) @String 254 "fe")) [SqlString "fe"]
    , (@?=) (unEnc @(R3.MakeR3 (R3.DateN 'OZ)) defEnc (R3.unsafeRefined3 (fromGregorian 2001 12 3) "2001-12-03")) [SqlString "2001-12-03"]
  -}
  ]

data T6 = T6 {t66 :: !Char, t666 :: !Double} deriving stock (G.Generic, Show)
instance GS.Generic T6
instance GS.HasDatatypeInfo T6
instance DefEnc (Enc T6)
