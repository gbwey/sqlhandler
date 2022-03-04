module TestSqlHelper where

import Database.HDBC (SqlValue (..))
import HSql.Core.Common

intRS1 :: Int -> ResultSet
intRS1 i = rset [[SqlInteger (fromIntegral i)]]

trueRS1 :: ResultSet
trueRS1 = rset [[true]]

falseRS1 :: ResultSet
falseRS1 = rset [[false]]

true, false :: SqlValue
true = SqlBool True
false = SqlBool False

rset :: [[SqlValue]] -> ResultSet
rset x = Right ([], x)
