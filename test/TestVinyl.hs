{-# OPTIONS -Wno-type-defaults #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module TestVinyl where

import qualified Control.Foldl as FL
import Control.Lens hiding (Const, Identity, rmap)
import qualified Control.Scanl as FS
import Data.String
import Data.Vinyl
import qualified Data.Vinyl.Functor as V
import HSql.Core.FrameUtils
import HSql.Core.One
import HSql.Core.VinylUtils
import Test.Tasty
import Test.Tasty.HUnit

statictests ::
  ( ( ToFields' Tst10T ~ Tst10T'
    , ToFields Tst10T'' ~ Tst10T'
    , ToFields (Int, Bool, Int) ~ '["c1" ::: Int, "c2" ::: Bool, "c3" ::: Int]
    , ToFields (One Int) ~ '["c1" ::: Int]
    ) =>
    ()
  ) ->
  ()
statictests x = x

suite :: TestTree
suite =
  testGroup
    "TestVinyl"
    [ testCase "rind1" $ (@?=) (rind' @1 tst9a) (Field True)
    , testCase "rind2" $ (@?=) (rind' @0 tst9a) (Field 'x')
    , testCase "rix1" $ (@?=) (I5 'd' True 14 "hello" 21 ^. rixI @2) 14
    , testCase "rix2" $ (@?=) (I5 'd' True 14 "hello" "abc" & rixI @3 <>~ "there") (I5 'd' True 14 "hellothere" "abc")
    , testCase "rix3" $ (@?=) (I4 'd' True 14 "hello" ^. rixI @0) 'd'
    , testCase "rix4" $ (@?=) (I4 'd' True 14 "hello" & rixI @0 %~ succ) (I4 'e' True 14 "hello")
    , testCase "rix5" $ (@?=) (tst10' & rix @1 %~ \(Field x) -> Field (not x)) tst10''
    , testCase "rix6" $ (@?=) (tst10' & rixF @1 %~ not) tst10''
    , testCase "rix7" $ (@?=) (tst10 & rixI @1 %~ not) tst10A
    , testCase "rix8" $ (@?=) (tst10 ^. rixI @1) True
    , testCase "rix9" $ (@?=) (tst9a & rix @1 .~ Field False) tst9a''
    , testCase "reclen1" $ (@?=) (recLenP tst10) 5
    , testCase "reclen2" $ (@?=) (recLen @Tst10T) 5
    , testCase "reclen3" $ (@?=) (recLen @'[Int, Char, Bool]) 3
    , testCase "reclen4" $ (@?=) (recLen @'["a" ::: Bool, "c" ::: Char]) 2
    , testCase "fold" $ (@?=) (FL.fold (FL.premap (rvalf #c) FL.sum) tst8) 15
    , testCase "scan1" $ (@?=) tst8a tst8a'
    , testCase "scan2" $ (@?=) tst8b tst8b'
    , testCase "ellens1" $ (@?=) ((#a =: True) & elLens %~ show . not) (#a =: "False")
    , testCase "ellens2" $ (@?=) ((#a =: True) & elLens1 @"b" %~ show . not) (#b =: "False") -- if wrong symbol will fail with compile time error!
    , testCase "defval1" $ (@?=) (defVal :: Rec ElField '["aa" ::: Bool]) (#aa =: False :& RNil)
    , testCase "defval2" $ (@?=) (defVal :: Rec ElField '["aa" ::: Bool, "bb" ::: Int, "cc" ::: String, "dd" ::: Maybe Int]) (#aa =: False :& #bb =: 0 :& #cc =: "" :& #dd =: Nothing :& RNil)
    , testCase "defval3" $ (@?=) (defVal :: Rec V.Identity '[Int, Bool, Maybe String]) (V.Identity 0 :& V.Identity False :& V.Identity Nothing :& RNil)
    ]

tst13 :: Rec ElField '[ '("aa", Bool), '("bb", Double), '("cc", Bool), '("aa", String)]
tst13 = #aa =: True :& #bb =: 123.45 :& #cc =: False :& #aa =: "asdf" :& RNil

tst13a :: Rec ElField '[ '("bb", Double), '("aa", String)]
tst13a = #bb =: 123.45 :& #aa =: "asdf" :& RNil

tst12 :: (Num nn, IsString ss) => Rec ElField '[ '("aa", Bool), '("bb", nn), '("cc", Char), '("aa", ss)]
tst12 = #aa =: True :& #bb =: 123 :& #cc =: 'x' :& #aa =: "asdf" :& RNil

tst12a :: (Num nn, IsString ss) => Rec ElField '[ '("aa", Bool), '("bb", nn), '("aa", ss)]
tst12a = #aa =: True :& #bb =: 123 :& #aa =: "asdf" :& RNil

tst11 :: Rec ElField '[ '("aa", Bool), '("bb", Double), '("cc", Char), '("aa", String)]
tst11 = #aa =: True :& #bb =: 123.45 :& #cc =: 'x' :& #aa =: "asdf" :& RNil

tst11a :: Rec ElField '[ '("bb", Double), '("cc", Char), '("aa", String)]
tst11a = #bb =: 123.45 :& #cc =: 'x' :& #aa =: "asdf" :& RNil

tst11b :: Rec ElField '[ '("bb", Double), '("cc", Char)]
tst11b = #bb =: 123.45 :& #cc =: 'x' :& RNil

tst11c :: Rec ElField '[ '("aa", Bool), '("cc", Char), '("aa", String)]
tst11c = #aa =: True :& #cc =: 'x' :& #aa =: "asdf" :& RNil

tst8a :: [Rec ElField '[ '("total", Int), '("a", Char), '("b", String), '("c", Int)]]
tst8a = FS.scan (postscanF @"total" (FL.premap (rvalf #c) FL.sum)) tst8
tst8b :: [Rec ElField '[ '("max", Maybe Int), '("a", Char), '("b", String), '("c", Int)]]
tst8b = FS.scan (postscanF @"max" (FL.premap (rvalf #c) FL.maximum)) tst8

tst8 :: [Rec ElField '[ '("a", Char), '("b", String), '("c", Int)]]
tst8 =
  [ Field 'x' :& Field "hello" :& Field 2 :& RNil
  , Field 'y' :& Field "aaa" :& Field 5 :& RNil
  , Field 'z' :& Field "bbb" :& Field 7 :& RNil
  , Field 'w' :& Field "ccc" :& Field 1 :& RNil
  ]

tst8a' :: [Rec ElField '[ '("total", Int), '("a", Char), '("b", String), '("c", Int)]]
tst8a' =
  [ Field 2 :& Field 'x' :& Field "hello" :& Field 2 :& RNil
  , Field 7 :& Field 'y' :& Field "aaa" :& Field 5 :& RNil
  , Field 14 :& Field 'z' :& Field "bbb" :& Field 7 :& RNil
  , Field 15 :& Field 'w' :& Field "ccc" :& Field 1 :& RNil
  ]

tst8b' :: [Rec ElField '[ '("max", Maybe Int), '("a", Char), '("b", String), '("c", Int)]]
tst8b' =
  [ Field (Just 2) :& Field 'x' :& Field "hello" :& Field 2 :& RNil
  , Field (Just 5) :& Field 'y' :& Field "aaa" :& Field 5 :& RNil
  , Field (Just 7) :& Field 'z' :& Field "bbb" :& Field 7 :& RNil
  , Field (Just 7) :& Field 'w' :& Field "ccc" :& Field 1 :& RNil
  ]

tst9 :: Rec ElField '[ '("a", Char), '("b", Bool)]
tst9 = Field 'x' :& Field True :& RNil

tst9a :: Rec ElField '[ '("a", Char), '("b", Bool), '("a", String), '("a", Int), '("b", Double)]
tst9a = Field 'x' :& Field True :& Field "hello" :& Field 44 :& Field 1.23 :& RNil

tst9a'' :: Rec ElField '[ '("a", Char), '("b", Bool), '("a", String), '("a", Int), '("b", Double)]
tst9a'' = Field 'x' :& Field False :& Field "hello" :& Field 44 :& Field 1.23 :& RNil

tst9a' :: Rec ElField '[ '("a", Char), '("b", Bool), '("a_1", String), '("a_2", Int), '("b_1", Double)]
tst9a' = Field 'x' :& Field True :& Field "hello" :& Field 44 :& Field 1.23 :& RNil

tst10 :: Rec V.Identity '[Char, Bool, String, Int, Double]
tst10 = I5 'x' True "hello" 44 1.23

tst10A :: Rec V.Identity '[Char, Bool, String, Int, Double]
tst10A = I5 'x' False "hello" 44 1.23

tst10' :: Rec ElField '[ '("c1", Char), '("c2", Bool), '("c3", String), '("c4", Int), '("c5", Double)]
tst10' = Field 'x' :& Field True :& Field "hello" :& Field 44 :& Field 1.23 :& RNil

tst10'' :: Rec ElField '[ '("c1", Char), '("c2", Bool), '("c3", String), '("c4", Int), '("c5", Double)]
tst10'' = Field 'x' :& Field False :& Field "hello" :& Field 44 :& Field 1.23 :& RNil

type Tst10T = '[Char, Bool, String, Int, Double]
type Tst10T' = '[ '("c1", Char), '("c2", Bool), '("c3", String), '("c4", Int), '("c5", Double)]
type Tst10T'' = (Char, Bool, String, Int, Double)
