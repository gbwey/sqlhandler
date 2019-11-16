-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}
module Main where
import qualified TestConv
import qualified TestEncoding
import qualified TestDecoding
import qualified TestSql
import qualified TestTablePrinter
import qualified TestSqlDeferred
import qualified TestVinyl
import Test.Hspec
import Test.Tasty

main :: IO ()
main = do
  hspec spec
  TestSqlDeferred.doit
  defaultMain $ testGroup "alltests"
    [ TestConv.suite
    , TestEncoding.suite
    , TestDecoding.suite
    , TestSql.suite
    , TestVinyl.suite
--  , TestTablePrinter.suite
--  , TestSqlDeferred.suite
    ]


spec :: Spec
spec = do
  describe "Conv"          TestConv.spec
  describe "Encoding"      TestEncoding.spec
  describe "Decoding"      TestDecoding.spec
  describe "TablePrinter"  TestTablePrinter.spec
  describe "Sql"           TestSql.spec
