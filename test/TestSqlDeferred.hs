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
        `shouldNotTypecheck (processRetCol invalidNested1 [])'
      In a stmt of a 'do' block:
        it "should not allow Alle within :+: or Some or Alle"
          $ shouldNotTypecheck (processRetCol invalidNested1 [])
      In the second argument of `($)', namely
        `do it "should not allow an Int to be a String"
              $ shouldNotTypecheck (4 :: String)
            it "should not allow Alle within :+: or Some or Alle"
              $ shouldNotTypecheck (processRetCol invalidNested1 [])
            it "should not allow Alle within :+: or Some or Alle"
              $ shouldNotTypecheck (processRetCol invalidNested2 [])
            it "should not allow Alle within :+: or Some or Alle"
              $ shouldNotTypecheck (processRetCol invalidNested3 [])
            ....'
   |
75 |       shouldNotTypecheck (processRetCol invalidNested1 [])
   |       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-}

-- actually usefull cos doesnt segfault: test on laptop to see if we get the same results
-- not so useful: need deepseq instance for Rec RState '[Alle Upd :+: Alle Upd]
{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# OPTIONS -Wno-deferred-type-errors #-}
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
import HSql.Core.Sql
import HSql.Core.VinylUtils
import HSql.Core.Decoder
import Test.ShouldNotTypecheck -- (shouldNotTypecheck)
import Test.Hspec -- (hspec, describe, it)
import Control.DeepSeq ()
import DeferredOrphans ()
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
invalidNested1 = E1 (AlleP UpdP :+: AlleP UpdP)

-- nested alle which is wrong
invalidNested2 :: Rec SingleIn '[Alle (Alle Upd)]
invalidNested2 = E1 (AlleP (AlleP UpdP))

invalidNested3 :: Rec SingleIn '[Alle (Upd :+: Alle Upd)]
invalidNested3 = E1 (AlleP (UpdP :+: AlleP UpdP))

invalidNested4 :: Rec SingleIn '[Upd :+: Alle Upd]
invalidNested4 = E1 (UpdP :+: AlleP UpdP)

-- use ValidABC
invalidAlle1 :: Rec SingleIn '[Alle Upd, Upd]
invalidAlle1 = E2 (AlleP UpdP) UpdP

invalidAlle2 :: Rec SingleIn '[SelOne Int, Alle Upd, Upd]
invalidAlle2 = E3 (SelOneP defDec) (AlleP UpdP) UpdP

invalidAlle3 :: Rec SingleIn '[Alle Upd, Alle Upd]
invalidAlle3 = E2 (AlleP UpdP) (AlleP UpdP)

invalidAlle4 :: Rec SingleIn '[Alle Upd, Some 'False 2 Upd]
invalidAlle4 = E2 (AlleP UpdP) (SomeP UpdP)

{-
on machine at home i get a segfault -- could be ghc or my crazy instances of NFData instead of using Generic more
-}

doit :: IO ()
doit = hspec $
  describe "Type Tests" $ do
    it "should not allow an Int to be a String" $
      shouldNotTypecheck (4 :: String)
    it "should not allow Alle within :+: or Some or Alle" $
      shouldNotTypecheck (processRetCol invalidNested1 [])
    it "should not allow Alle within :+: or Some or Alle" $
      shouldNotTypecheck (processRetCol invalidNested2 [])
    it "should not allow Alle within :+: or Some or Alle" $
      shouldNotTypecheck (processRetCol invalidNested3 [])
    it "should not allow Alle within :+: or Some or Alle" $
      shouldNotTypecheck (processRetCol invalidNested4 [])

    it "should not allow Alle/Some unless last and only one" $
      shouldNotTypecheck (processRetCol invalidAlle1 [])
    it "should not allow Alle/Some unless last and only one" $
      shouldNotTypecheck (processRetCol invalidAlle2 [])
    it "should not allow Alle/Some unless last and only one" $
      shouldNotTypecheck (processRetCol invalidAlle3 [])
    it "should not allow Alle/Some unless last and only one" $
      shouldNotTypecheck (processRetCol invalidAlle4 [])

--    it "should not allow Alle/Some unless last and only one" $
--       shouldNotTypecheck $ processRetCol valid6 []

