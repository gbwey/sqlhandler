-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}
module Main where
import qualified TestConv
import qualified TestEncoder
import qualified TestDecoder
import qualified TestSql
import qualified TestTablePrinter
import qualified TestSqlDeferred
import qualified TestVinyl
import Test.Hspec
import Test.Tasty
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  hspec spec
  TestSqlDeferred.doit
  defaultMain $ testGroup "alltests"
    [ TestConv.suite
    , TestEncoder.suite
    , TestDecoder.suite
    , TestSql.suite
    , TestVinyl.suite
--  , TestTablePrinter.suite
--  , TestSqlDeferred.suite
    ]


spec :: Spec
spec = do
  describe "Conv"          TestConv.spec
  describe "Encoder"       TestEncoder.spec
  describe "Decoder"       TestDecoder.spec
  describe "TablePrinter"  TestTablePrinter.spec
  describe "Sql"           TestSql.spec
