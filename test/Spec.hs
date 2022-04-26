module Main where

import System.IO
import Test.Hspec
import Test.Tasty
import qualified TestConv
import qualified TestDecoder
import qualified TestEncoder
import qualified TestOperator
import qualified TestRStateBuilder
import qualified TestSql
import qualified TestSqlDeferred
import qualified TestSqlParserMS
import qualified TestTablePrinter
import qualified TestVinyl

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  hspec spec
  TestSqlDeferred.doit
  defaultMain $
    testGroup
      "alltests"
      [ TestConv.suite
      , TestEncoder.suite
      , TestDecoder.suite
      , TestSql.suite
      , TestOperator.suite
      , TestRStateBuilder.suite
      , TestVinyl.suite
      , TestSqlParserMS.suite
      ]

spec :: Spec
spec = do
  describe "Conv" TestConv.spec
  describe "Encoder" TestEncoder.spec
  describe "Decoder" TestDecoder.spec
  describe "TablePrinter" TestTablePrinter.spec
  describe "Sql" TestSql.spec
