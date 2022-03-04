{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module TestRStateBuilder where

import HSql.Core
import HSql.Core.RStateBuilder
import HSql.Core.SqlBuilder
import Test.Tasty
import Test.Tasty.HUnit

-- selraw to sel (a,b,c) to selrowcol a
-- flattenAlleSel: Alle Sel to Sel -- test
-- flattenSomeSelRow: Some SelRow to Sel
-- selColToSelRowCol
-- selToSelRow
-- selRowColToSelCol

xx, xxBad, xxRow :: RState SelRaw
xx = mkRState $ mkSelRaw ["A", "B"] [[SqlInt32 10, SqlString "Abc"], [SqlInt32 12, SqlString "Def"]]
xxBad = mkRState $ mkSelRaw ["A", "B"] [[SqlInt32 10, SqlString "Abc"], [SqlInt32 12]]
xxRow = mkRState $ mkSelRaw ["A", "B"] [[SqlInt32 10, SqlString "Abc"]]

yy :: Either String (RState (Sel (Int, String)))
yy = selRawToSel @(Int, String) xx

yyBad :: Either String (RState (Sel (Int, String)))
yyBad = selRawToSel @(Int, String) xxBad

yyRow :: Either String (Either String (RState (SelRow (Int, String))))
yyRow = selToSelRow <$> selRawToSel @(Int, String) xxRow

yyRowBad :: Either String (Either String (RState (SelRow (Int, String))))
yyRowBad = selToSelRow <$> selRawToSel @(Int, String) xx

suite :: TestTree
suite =
  testGroup
    "TestRStateBuilder"
    [ testCase "SelRaw to Sel to SelRaw" $ fmap selToSelRaw (selRawToSel @(Int, String) xx) @?= Right xx
    ]
