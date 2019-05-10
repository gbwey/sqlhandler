-- DONE: need to fix this one!
{-
this fixed it: defined for Data.Functor.Identity but not vinyl Identity
deriving instance Generic1 V.Identity
instance NFData1 V.Identity
instance NFData a => NFData (V.Identity a)


D:\haskell\sqlhandler\test\TestSqlDeferred.hs:75:7: warning: [-Wdeferred-type-errors]
    * No instance for (Control.DeepSeq.NFData
                         (Data.Vinyl.Functor.Identity UnconsumedE))
        arising from a use of `shouldNotTypecheck'
      There are instances for similar types:
        instance [safe] Control.DeepSeq.NFData a =>
                        Control.DeepSeq.NFData (Data.Functor.Identity.Identity a)
          -- Defined in `Control.DeepSeq'
    * In the second argument of `($)', namely
        `shouldNotTypecheck (processRet invalidNested1 [])'
      In a stmt of a 'do' block:
        it "should not allow Alle within :+: or Some or Alle"
          $ shouldNotTypecheck (processRet invalidNested1 [])
      In the second argument of `($)', namely
        `do it "should not allow an Int to be a String"
              $ shouldNotTypecheck (4 :: String)
            it "should not allow Alle within :+: or Some or Alle"
              $ shouldNotTypecheck (processRet invalidNested1 [])
            it "should not allow Alle within :+: or Some or Alle"
              $ shouldNotTypecheck (processRet invalidNested2 [])
            it "should not allow Alle within :+: or Some or Alle"
              $ shouldNotTypecheck (processRet invalidNested3 [])
            ....'
   |
75 |       shouldNotTypecheck (processRet invalidNested1 [])
   |       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-}

-- actually usefull cos doesnt segfault: test on laptop to see if we get the same results
-- not so useful: need deepseq instance for Rec ZZZ '[Alle Upd :+: Alle Upd]
{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# OPTIONS -Wall -Wcompat -Wincomplete-record-updates -Wincomplete-uni-patterns -Wredundant-constraints #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module TestSqlDeferred where
import Data.Vinyl
import Sql
import Test.ShouldNotTypecheck -- (shouldNotTypecheck)
import Test.Hspec -- (hspec, describe, it)
import Control.DeepSeq ()
import DeferredOrphans ()
import PredState
import Frames () -- has NFData instances for V.Identity and others (extrainstances): else will have to add them back to DeferredOrphans
-- todo: NOTE: interesting error: Proxy @NFData
{- but is cos it was missing import Data.Proxy!!
C:\haskell\sqlhandler\src\OrphanInstances.hs:24:102: error:
    * Cannot apply expression of type `t1'
      to a visible type argument `NFData'
    * In the first argument of `reifyConstraint', namely
        `(Proxy @NFData)'
      In the second argument of `($)', namely
        `reifyConstraint (Proxy @NFData) w'
      In the second argument of `($)', namely
        `rmap (\ (V.Compose (Dict x)) -> V.Const $ rnf x)
         $ reifyConstraint (Proxy @NFData) w'
-}

-- invalid! but is allowed thru!
invalidNested1 :: Rec SingleIn '[Alle Upd :+: Alle Upd]
invalidNested1 = E1 (AlleP (UpdP ptrue) ptrue :+: AlleP (UpdP ptrue) ptrue)

-- nested alle which is wrong
invalidNested2 :: Rec SingleIn '[Alle (Alle Upd)]
invalidNested2 = E1 (AlleP (AlleP (UpdP ptrue) ptrue) ptrue)

invalidNested3 :: Rec SingleIn '[Alle (Upd :+: Alle Upd)]
invalidNested3 = E1 (AlleP (UpdP ptrue :+: AlleP (UpdP ptrue) ptrue) ptrue)

invalidNested4 :: Rec SingleIn '[Upd :+: Alle Upd]
invalidNested4 = E1 (UpdP ptrue :+: AlleP (UpdP ptrue) ptrue)

-- use ValidABC
invalidAlle1 :: Rec SingleIn '[Alle Upd, Upd]
invalidAlle1 = E2 (AlleP (UpdP ptrue) ptrue) (UpdP ptrue)

invalidAlle2 :: Rec SingleIn '[SelOne Int, Alle Upd, Upd]
invalidAlle2 = E3 (SelOneP ptrue defDec) (AlleP (UpdP ptrue) ptrue) (UpdP ptrue)

invalidAlle3 :: Rec SingleIn '[Alle Upd, Alle Upd]
invalidAlle3 = E2 (AlleP (UpdP ptrue) ptrue) (AlleP (UpdP ptrue) ptrue)

invalidAlle4 :: Rec SingleIn '[Alle Upd, Some 2 Upd]
invalidAlle4 = E2 (AlleP (UpdP ptrue) ptrue) (SomeP (UpdP ptrue) ptrue)

{-
on machine at home i get a segfault -- could be ghc or my crazy instances of NFData instead of using Generic more
-}

main :: IO ()
main = hspec $
  describe "Type Tests" $ do
    it "should not allow an Int to be a String" $
      shouldNotTypecheck (4 :: String)
    it "should not allow Alle within :+: or Some or Alle" $
      shouldNotTypecheck (processRet invalidNested1 [])
    it "should not allow Alle within :+: or Some or Alle" $
      shouldNotTypecheck (processRet invalidNested2 [])
    it "should not allow Alle within :+: or Some or Alle" $
      shouldNotTypecheck (processRet invalidNested3 [])
    it "should not allow Alle within :+: or Some or Alle" $
      shouldNotTypecheck (processRet invalidNested4 [])

    it "should not allow Alle/Some unless last and only one" $
      shouldNotTypecheck (processRet invalidAlle1 [])
    it "should not allow Alle/Some unless last and only one" $
      shouldNotTypecheck (processRet invalidAlle2 [])
    it "should not allow Alle/Some unless last and only one" $
      shouldNotTypecheck (processRet invalidAlle3 [])
    it "should not allow Alle/Some unless last and only one" $
      shouldNotTypecheck (processRet invalidAlle4 [])

--    it "should not allow Alle/Some unless last and only one" $
--       shouldNotTypecheck $ processRet valid6 []

