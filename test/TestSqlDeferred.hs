{-# OPTIONS -Wno-deferred-type-errors #-}
{-# OPTIONS -Wno-unused-imports #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}

module TestSqlDeferred where

import Control.DeepSeq
import qualified Control.Exception as E
import DeferredOrphans ()
import HSql.Core.Decoder
import HSql.Core.Sql
import Test.Hspec
import Test.ShouldNotTypecheck

doit :: IO ()
doit = hspec $
  describe "Type Tests" $ do
    it "should not allow an Int to be a String" $
      shouldNotTypecheck (4 :: String)
    it "should not allow Alle within :+: or Exact or Alle" $
      shouldNotTypecheck (processRetCol @'[Alle Upd :+: Alle Upd] defDec [])
    it "should not allow Alle within :+: or Exact or Alle" $
      shouldNotTypecheck (processRetCol @'[Alle (Alle Upd)] defDec [])
    it "should not allow Alle within :+: or Exact or Alle" $
      shouldNotTypecheck (processRetCol @'[Alle (Upd :+: Alle Upd)] defDec [])
    it "should not allow Alle within :+: or Exact or Alle" $
      shouldNotTypecheck (processRetCol @'[Upd :+: Alle Upd] defDec [])
    it "should not allow Alle/Exact unless last and only one" $
      shouldNotTypecheck (processRetCol @'[Alle Upd, Upd] defDec [])
    it "should not allow Alle/Exact unless last and only one" $
      shouldNotTypecheck (processRetCol @'[SelRow Int, Alle Upd, Upd] defDec [])
    it "should not allow Alle/Exact unless last and only one" $
      shouldNotTypecheck (processRetCol @'[Alle Upd, Alle Upd] defDec [])
    it "should not allow Alle/Exact unless last and only one" $
      shouldNotTypecheck (processRetCol @'[Alle Upd, Exact 2 Upd] defDec [])
