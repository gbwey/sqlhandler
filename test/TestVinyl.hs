-- rind rind' are also in predicate/vinylhelper but less generic definition
-- also has Pattern E1 etc
{-# OPTIONS -Wall -Wcompat -Wincomplete-record-updates -Wincomplete-uni-patterns -Wredundant-constraints #-}
{-# OPTIONS -Wno-type-defaults #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
module TestVinyl where
import qualified Data.Vinyl.Functor as V
import Data.Vinyl
import Control.Lens hiding (rmap,Identity,Const)
import EasyTest
--import GHC.TypeLits hiding (natVal, natVal')
--import GHC.TypeNats
import qualified PCombinators as P
import VinylUtils
import qualified Control.Foldl as FL
import qualified Control.Scanl as FS
import One
import Data.String

statictests :: ((
         ToFields' Tst10T ~ Tst10T'
        ,ToFields Tst10T'' ~ Tst10T'
        ,ToFields (Int, Bool, Int) ~ '["c1" ::: Int, "c2" ::: Bool, "c3" ::: Int]
        ,ToFields (One Int) ~ '["c1" ::: Int]
        ,P.Fst (P.Snd (P.UnSnoc' (ToFields' (P.Mconcat (P.Replicate 11 '[Int,Bool]))))) ~ "c22"
       ) => ()) -> ()
statictests  x = x

statictests1 :: ()
statictests1  =
          [ removeField @"aa" tst11a == tst11b
          , removeField @"zz" tst11 == tst11
          , removeField @"aa" tst11 == tst11a
          , removeField @"aa" tst11a == tst11b
          , removeType @Bool tst11 == tst11a
          , removeType @Double tst11 == tst11c
          , removeType @(Maybe _) tst11 == tst11
          , removeTypes @Bool tst13 == tst13a
          ] `seq` ()

main :: IO ()
main = run suite

-- rix rind makef reclen recget postscanF prescanF uniqueRec
suite :: Test ()
suite = tests
  [ scope "uniq1" $ expectEq (uniqueRec tst9a) tst9a'
  , scope "uniq2" $ expectEq (uniqueRec tst9) tst9
  , scope "uniq3" $ expectEq (uniqueRec (#a =: True :& #b =: 'x' :& #a =: "asdf" :& RNil)) (uniqueRec (#a =: True :& #b =: 'x' :& #a_1 =: "asdf" :& RNil))
  , scope "uniq4" $ expectEq (uniqueRec (#a =: True :& #b =: 'x' :& #c =: "asdf" :& RNil)) (uniqueRec (#a =: True :& #b =: 'x' :& #c =: "asdf" :& RNil))
  , scope "rind1" $ expectEq (rind' @1 tst9a) (Field @"b" True)
  , scope "rind2" $ expectEq (rind' @0 tst9a) (Field @"a" 'x')
  , scope "rix1" $ expectEq (I5 'd' True 14 "hello" 21 ^. rixI @2) 14
  , scope "rix2" $ expectEq (I5 'd' True 14 "hello" "abc" & rixI @3 <>~ "there") (I5 'd' True 14 "hellothere" "abc")
  , scope "rix3" $ expectEq (I4 'd' True 14 "hello" ^. rixI @0) 'd'
  , scope "rix4" $ expectEq (I4 'd' True 14 "hello" & rixI @0 %~ succ) (I4 'e' True 14 "hello")
  , scope "rix5" $ expectEq (tst10' & rix @1 %~ \(Field x) -> Field (not x)) tst10''
  , scope "rix6" $ expectEq (tst10' & rixF @1 %~ not) tst10''
  , scope "rix7" $ expectEq (tst10 & rixI @1 %~ not) tst10A
  , scope "rix8" $ expectEq (tst10 ^. rixI @1) True
  , scope "rix9" $ expectEq (tst9a & rix @1 .~ Field False) tst9a''
  , scope "reclen1" $ expectEq (recLenP tst10) 5
  , scope "reclen2" $ expectEq (recLen @Tst10T) 5
  , scope "reclen3" $ expectEq (recLen @'[Int,Char,Bool]) 3
  , scope "reclen4" $ expectEq (recLen @'["a" ::: Bool, "c" ::: Char]) 2
  , scope "fold" $ expectEq (FL.fold (FL.premap (rvalf #c) FL.sum) tst8) 15
  , scope "scan1" $ expectEq tst8a tst8a'
  , scope "scan2" $ expectEq tst8b tst8b'
  , scope "ellens" $ expectEq ((#a =: True) & elLens  %~ show . not) (#a =: "False")
  , scope "ellens1" $ expectEq ((#a =: True) & elLens1 @"b" %~ show . not) (#b =: "False") -- if wrong symbol will fail with compile time error!
  , scope "defval1" $ expectEq (defVal :: Rec ElField '["aa" ::: Bool]) (#aa =: False :& RNil)
  , scope "defval2" $ expectEq (defVal :: Rec ElField '["aa" ::: Bool, "bb" ::: Int, "cc" ::: String, "dd" ::: Maybe Int]) (#aa =: False :& #bb =: 0 :& #cc =: "" :& #dd =: Nothing :& RNil)
  , scope "defval3" $ expectEq (defVal :: Rec V.Identity '[Int,Bool,Maybe String]) (V.Identity 0 :& V.Identity False :& V.Identity Nothing :& RNil)
--  , scope "removetype4" $ expectEq (removeType @Char tst12) tst12a
  ]

tst13 :: Rec ElField '[ '("aa", Bool), '("bb", Double), '("cc", Bool), '("aa", String) ]
tst13 = #aa =: True :& #bb =: 123.45 :& #cc =: False :& #aa =: "asdf" :& RNil

tst13a :: Rec ElField '[ '("bb", Double), '("aa", String) ]
tst13a = #bb =: 123.45 :& #aa =: "asdf" :& RNil


--ww :: (Num nn, IsString ss) => Rec ElField '[ '("aa", Bool), '("bb", nn), '("aa", ss) ]
--ww = removeType @Char tst12

tst12 :: (Num nn, IsString ss) => Rec ElField '[ '("aa", Bool), '("bb", nn), '("cc", Char), '("aa", ss) ]
tst12 = #aa =: True :& #bb =: 123 :& #cc =: 'x' :& #aa =: "asdf" :& RNil

tst12a :: (Num nn, IsString ss) => Rec ElField '[ '("aa", Bool), '("bb", nn), '("aa", ss) ]
tst12a = #aa =: True :& #bb =: 123 :& #aa =: "asdf" :& RNil


tst11 :: Rec ElField '[ '("aa", Bool), '("bb", Double), '("cc", Char), '("aa", String) ]
tst11 = #aa =: True :& #bb =: 123.45 :& #cc =: 'x' :& #aa =: "asdf" :& RNil

tst11a :: Rec ElField '[ '("bb", Double), '("cc", Char), '("aa", String) ]
tst11a = #bb =: 123.45 :& #cc =: 'x' :& #aa =: "asdf" :& RNil

tst11b :: Rec ElField '[ '("bb", Double), '("cc", Char) ]
tst11b = #bb =: 123.45 :& #cc =: 'x' :& RNil

tst11c :: Rec ElField '[ '("aa", Bool), '("cc", Char), '("aa", String) ]
tst11c = #aa =: True :& #cc =: 'x' :& #aa =: "asdf" :& RNil


tst8a :: [Rec ElField '[ '("total", Int), '("a", Char), '("b", String), '("c",Int) ] ]
tst8a = FS.scan (postscanF @"total" (FL.premap (rvalf #c) FL.sum)) tst8
tst8b :: [Rec ElField '[ '("max", Maybe Int), '("a", Char), '("b", String), '("c",Int) ] ]
tst8b = FS.scan (postscanF @"max" (FL.premap (rvalf #c) FL.maximum)) tst8
--tst8apre = FS.scan (prescanF @"total" (FL.premap (rvalf #c) FL.sum)) tst8
--tst8bpre = FS.scan (prescanF @"max" (FL.premap (rvalf #c) FL.maximum)) tst8

tst8 :: [Rec ElField '[ '("a", Char), '("b", String), '("c",Int) ] ]
tst8 = [Field 'x' :& Field "hello" :& Field 2 :& RNil
       ,Field 'y' :& Field "aaa" :& Field 5 :& RNil
       ,Field 'z' :& Field "bbb" :& Field 7 :& RNil
       ,Field 'w' :& Field "ccc" :& Field 1 :& RNil]

tst8a' :: [Rec ElField '[ '("total", Int), '("a", Char), '("b", String), '("c",Int) ] ]
tst8a' =
       [Field 2 :& Field 'x' :& Field "hello" :& Field 2 :& RNil
       ,Field 7 :& Field 'y' :& Field "aaa" :& Field 5 :& RNil
       ,Field 14 :& Field 'z' :& Field "bbb" :& Field 7 :& RNil
       ,Field 15 :& Field 'w' :& Field "ccc" :& Field 1 :& RNil
       ]

tst8b' :: [Rec ElField '[ '("max", Maybe Int), '("a", Char), '("b", String), '("c",Int) ] ]
tst8b' =
       [Field (Just 2) :& Field 'x' :& Field "hello" :& Field 2 :& RNil
       ,Field (Just 5) :& Field 'y' :& Field "aaa" :& Field 5 :& RNil
       ,Field (Just 7) :& Field 'z' :& Field "bbb" :& Field 7 :& RNil
       ,Field (Just 7) :& Field 'w' :& Field "ccc" :& Field 1 :& RNil
       ]

tst9 :: Rec ElField '[ '("a", Char), '("b", Bool) ]
tst9 = Field 'x' :& Field True :& RNil

tst9a :: Rec ElField '[ '("a", Char), '("b", Bool), '("a", String), '("a",Int), '("b",Double) ]
tst9a = Field 'x' :& Field True :& Field "hello" :& Field 44 :& Field 1.23 :& RNil

tst9a'' :: Rec ElField '[ '("a", Char), '("b", Bool), '("a", String), '("a",Int), '("b",Double) ]
tst9a'' = Field 'x' :& Field False :& Field "hello" :& Field 44 :& Field 1.23 :& RNil

tst9a' :: Rec ElField '[ '("a", Char), '("b", Bool), '("a_1", String), '("a_2",Int), '("b_1",Double) ]
tst9a' = Field 'x' :& Field True :& Field "hello" :& Field 44 :& Field 1.23 :& RNil

tst10 :: Rec V.Identity '[Char, Bool, String, Int, Double]
tst10 = I5 'x' True "hello" 44 1.23

tst10A :: Rec V.Identity '[Char, Bool, String, Int, Double]
tst10A = I5 'x' False"hello" 44 1.23

tst10' :: Rec ElField '[ '("c1", Char), '("c2", Bool), '("c3", String), '("c4",Int), '("c5",Double) ]
tst10' = Field 'x' :& Field True :& Field "hello" :& Field 44 :& Field 1.23 :& RNil

tst10'' :: Rec ElField '[ '("c1", Char), '("c2", Bool), '("c3", String), '("c4",Int), '("c5",Double) ]
tst10'' = Field 'x' :& Field False :& Field "hello" :& Field 44 :& Field 1.23 :& RNil

type Tst10T = '[Char, Bool, String, Int, Double]
type Tst10T' = '[ '("c1", Char), '("c2", Bool), '("c3", String), '("c4",Int), '("c5",Double) ]
type Tst10T'' = (Char, Bool, String, Int, Double)



