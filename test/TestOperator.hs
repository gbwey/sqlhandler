{-# OPTIONS -Wno-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module TestOperator where

import Control.Arrow
import Control.Monad
import Data.Either
import Data.Fix
import Data.Function
import Data.List.NonEmpty (NonEmpty (..))
import Data.Tuple.Extra
import DocUtils.Doc
import HSql.Core.Operator
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import qualified Test.QuickCheck.Property as QP
import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as TQ

newtype GenInput = GenInput (Maybe Int) deriving newtype (Show, Eq)

instance Arbitrary GenInput where
  arbitrary =
    GenInput <$> do
      frequency [(1, pure Nothing), (7, Just <$> choose (-8, 8))]

genString :: Gen String
genString = replicateM 3 $ choose ('A', 'Z')

instance Arbitrary1 ExprF where
  liftArbitrary ga = do
    a <- ga
    b <- ga
    oneof
      [ EBool <$> arbitrary
      , ELT <$> arbitrary
      , ELE <$> arbitrary
      , EEQ <$> arbitrary
      , EGE <$> arbitrary
      , EGT <$> arbitrary
      , ENE <$> arbitrary
      , EBetween <$> arbitrary <*> arbitrary
      , do
          nm <- choose (0, 4)
          zs <- replicateM nm arbitrary
          return $ case zs of
            [] -> EBool False
            x : xs -> EElem (x :| xs)
      , EGroup <$> genString <*> pure a
      , pure $ EAnd a b
      , pure $ EOr a b
      , pure $ ESame a b
      , pure $ EXor a b
      ]
  liftShrink shr = go
   where
    go = \case
      ENot x -> ENot <$> shr x
      EAnd x y -> EAnd <$> shr x <*> shr y
      EOr x y -> EOr <$> shr x <*> shr y
      ESame x y -> ESame <$> shr x <*> shr y
      EXor x y -> EXor <$> shr x <*> shr y
      EGroup s x -> EGroup s <$> shr x
      _atom -> []

data LR
  = L1 String OpRet OpRet
  | L2 String OpRet OpRet [(Int, OpRet)]
  | L3 String OpRet [(Int, OpRet)]
  | L4 String [(Int, (OpRet, OpRet))]
  deriving stock (Show)

-- works best
assertSimplify :: Int -> IO ()
assertSimplify cnt = do
  errs <- lefts <$> replicateM cnt testA'
  case errs of
    [] -> return ()
    _ : _ -> do
      mapM_ print errs
      assertFailure $ "simplify expresssion failed: with " ++ show (length errs) ++ " errors\n" ++ psiS errs

testA' :: IO (Either ((Expr, Expr), LR) (Expr, Expr))
testA' = do
  e0 <- generate (arbitrary @Expr)
  let e1 = simplifyE e0
  let ret = (e0, e1)
  return $ ((ret,) +++ const ret) (testSimplify e0 e1)

qcSimplify :: IO ()
qcSimplify = quickCheckWith stdArgs{maxSize = 8, maxSuccess = 1_000} simplifyResult

simplifyResult :: Expr -> QP.Result
simplifyResult e0 =
  foldr
    ( \mi k x ->
        let e1 = simplifyE e0
            a0 = evalE mi e0
            a1 = evalE mi e1
         in case (mi, opBool a0, opBool a1) of
              (Nothing, Nothing, Just _) -> k x
              _fail ->
                if on (==) opBool a0 a1
                  then k x
                  else QP.failed{QP.reason = "input=" ++ show mi ++ " " ++ opExpr a0 ++ " /= " ++ opExpr a1}
    )
    id
    (Nothing : map Just [-8 .. 8])
    QP.succeeded

testSimplify :: Expr -> Expr -> Either LR ()
testSimplify e0 e1 = do
  let a0 = evalE Nothing e0
      a1 = evalE Nothing e1
      tps = map (\i -> (i, both (evalE (Just i)) (e0, e1))) [-8 .. 8]
      ans0 = map (\(i, (x, _)) -> (i, x)) tps
      ans1 = map (\(i, (_, y)) -> (i, y)) tps
  case opBool a0 of
    x@(Just _)
      | opBool a1 /= x -> Left $ L1 "Nothing case for a0 has a Just but is different on simplication" a0 a1
      | otherwise ->
          case filter ((/= x) . opBool . snd) ans0 of
            [] -> Right ()
            xs@(_ : _) -> Left $ L2 "Nothing case for a0 has a Just but doesnt match a1" a0 a1 xs
    Nothing -> Right ()
  case opBool a1 of
    x@(Just _) -> case filter ((/= x) . opBool . snd) ans1 of
      [] -> Right ()
      xs@(_ : _) -> Left $ L3 "in Nothing case for e1 has a Just but didnt match with all the values" a1 xs
    _ok -> Right ()
  case filter (uncurry (on (/=) opBool) . snd) tps of
    [] -> Right ()
    xs@(_ : _) -> Left $ L4 "original doesnt match simplified" xs

suite :: TestTree
suite =
  testGroup "TestOperator" $
    zipWith
      (\i -> testCase ("tst" ++ show i))
      [1 :: Int ..]
      [ evalC @( 'OGT ( 'SPos 4)) (Just 10) @?= OpRet (Just True) "10 > 4" []
      , evalC @( 'OGT ( 'SPos 4) ':&&: 'OLE ( 'SPos 10)) (Just 11) @?= OpRet (Just False) "(11 > 4 && 11 <= 10)" ["11 <= 10"]
      , evalC @( 'OGT ( 'SPos 4) ':&&: 'OLE ( 'SPos 10)) (Just 10) @?= OpRet (Just True) "(10 > 4 && 10 <= 10)" []
      , evalC @( 'OElem ( 'SPos 2 ':| '[ 'SPos 7, 'SPos 19])) (Just 10) @?= OpRet (Just False) "10 `elem` [2,7,19]" ["10 `elem` [2,7,19]"]
      , evalC @( 'OElem ( 'SPos 2 ':| '[ 'SPos 7, 'SPos 19])) (Just 7) @?= OpRet (Just True) "7 `elem` [2,7,19]" []
      , evalC @( 'OElem ( 'SNeg 2 ':| '[ 'SNeg 7, 'SNeg 19])) (Just (-7)) @?= OpRet (Just True) "-7 `elem` [-2,-7,-19]" []
      , evalC @( 'OEQ ( 'SNeg 10)) (Just 10) @?= OpRet (Just False) "10 == -10" ["10 == -10"]
      , evalC @( 'OEQ ( 'SNeg 10)) (Just (-10)) @?= OpRet (Just True) "-10 == -10" []
      , evalC @( 'OLT ( 'SNeg 10)) (Just (-14)) @?= OpRet (Just True) "-14 < -10" []
      , testOks @( 'OGE ( 'SNeg 10)) (map Just [4, -4, 9, -9, -10])
      , testFails @( 'OGE ( 'SNeg 10)) (map Just [-11, -12])
      , evalC @( 'OGT ( 'SNeg 10)) (Just (-14)) @?= OpRet (Just False) "-14 > -10" ["-14 > -10"]
      , evalC @( 'OElem ( 'SNeg 14 ':| '[ 'SNeg 13])) (Just (-14)) @?= OpRet (Just True) "-14 `elem` [-14,-13]" []
      , testOks @( 'OGT ( 'SPos 4) ':&&: 'OLT ( 'SPos 7)) (map Just [5 .. 6])
      , testFails @( 'OGT ( 'SPos 4) ':&&: 'OLT ( 'SPos 7)) (map Just [3, 4, 7, 8])
      , testOks @( 'OEQ ( 'SPos 4) ':||: 'OElem ( 'SPos 7 ':| '[ 'SPos 8, 'SPos 9])) (map Just [4, 7, 8, 9])
      , testFails @( 'ONot ( 'OEQ ( 'SPos 4) ':||: 'OElem ( 'SPos 7 ':| '[ 'SPos 8, 'SPos 9]))) (map Just [4, 7, 8, 9])
      , evalC @ 'OTrue (Just 1) @?= OpRet (Just True) "True" []
      , evalC @ 'OFalse (Just 1) @?= OpRet (Just False) "False" ["False"]
      , evalC @( 'ONE ( 'SPos 7)) (Just 1) @?= OpRet (Just True) "1 /= 7" []
      , evalC @( 'OGroup "test" ( 'ONE ( 'SPos 7))) (Just 7) @?= OpRet (Just False) "{test}(7 /= 7)" ["7 /= 7"]
      , testFails @( 'OLT ( 'SPos 4) ':==: 'OElem ( 'SPos 1 ':| '[ 'SPos 7, 'SPos 8, 'SPos 9, 'SPos 2])) (map Just [3, 7, 8])
      , testOks @( 'OLT ( 'SPos 4) ':==: 'OElem ( 'SPos 1 ':| '[ 'SPos 7, 'SPos 8, 'SPos 9, 'SPos 2])) (map Just [1, 2])
      , testOks @( 'OLT ( 'SPos 4) ':/=: 'OElem ( 'SPos 1 ':| '[ 'SPos 7, 'SPos 8, 'SPos 9, 'SPos 2])) (map Just [3, 7, 8])
      , testFails @( 'OLT ( 'SPos 4) ':/=: 'OElem ( 'SPos 1 ':| '[ 'SPos 7, 'SPos 8, 'SPos 9, 'SPos 2])) (map Just [1, 2])
      , evalC @(OGT 4 ':&&: OLE 10) (Just 11) @?= OpRet (Just False) "(11 > 4 && 11 <= 10)" ["11 <= 10"]
      , evalC @(OGT 3 ':||: ONE 7) (Just 4) @?= OpRet (Just True) "(4 > 3 || 4 /= 7)" []
      , evalC @(OGT 3 ':||: ONE 7) Nothing @?= OpRet Nothing "(rc > 3 || rc /= 7)" []
      , testOks @( 'SNeg 4 ':-: 'SPos 7) (map Just [-4 .. 7])
      , testFails @( 'SNeg 4 ':-: 'SPos 7) (map Just ([-7 .. -5] ++ [8 .. 10]))
      , evalC @( 'SNeg 4 ':-: 'SPos 7) (Just (-5)) @?= OpRet (Just False) "-4 <= -5 <= 7" ["-5 >= -4"]
      , evalC @(4 :-: 7) (Just 5) @?= OpRet (Just True) "4 <= 5 <= 7" []
      , evalC @(4 :-: 7) (Just 1) @?= OpRet (Just False) "4 <= 1 <= 7" ["1 >= 4"]
      , evalC @(4 :-: 7) (Just 8) @?= OpRet (Just False) "4 <= 8 <= 7" ["8 <= 7"]
      , evalC @(4 :-: 7) Nothing @?= OpRet Nothing "4 <= rc <= 7" []
      , evalC @( 'OFalse ':&&: OGT 7) Nothing @?= OpRet Nothing "(False && rc > 7)" ["False"]
      , simplifyEvalC @( 'OFalse ':&&: OGT 7) Nothing @?= OpRet (Just False) "False" ["False"]
      , simplifyC @( 'OFalse ':&&: OGT 7) @?= Fix (EBool False)
      , simplifyC @(4 :-: 7 ':&&: 11 :-: 13) @?= Fix (EBool False)
      , evalC @( 'OFalse ':&&: OGT 7) Nothing @?= OpRet Nothing "(False && rc > 7)" ["False"]
      , evalC @(OElem (1 ':| '[2, 3]) ':||: 'OFalse ':&&: OGT 7) Nothing @?= OpRet Nothing "(rc `elem` [1,2,3] || (False && rc > 7))" ["False"]
      , evalC @(OElem (1 ':| '[2, 3]) ':||: 'OTrue ':&&: OGT 7) Nothing @?= OpRet Nothing "(rc `elem` [1,2,3] || (True && rc > 7))" []
      , evalC @( 'ONot (OElem (1 ':| '[2, 3]) ':||: 'OTrue ':&&: OGT 7)) Nothing @?= OpRet Nothing "not (rc `elem` [1,2,3] || (True && rc > 7))" []
      , simplifyEvalC @(OLT 12 ':&&: ( 'OFalse ':&&: OGT 4)) Nothing @?= OpRet (Just False) "False" ["False"]
      , simplifyEvalC @(OLT 12 ':&&: ( 'OFalse ':&&: OGT 4)) (Just 7) @?= OpRet (Just False) "False" ["False"]
      , opPretty True (simplifyEvalC @(OGT 3 ':&&: OLT 7) (Just 8)) @?= "False: [ 4 <= 8 <= 6 ] => failures: [ 8 <= 6 ]"
      , simplifyC @(6 :-: 5 ':||: 3 :-: 3) @?= EEQP 3
      , simplifyC @(6 :-: 5 ':&&: 3 :-: 3) @?= EFalseP
      , simplifyC @(3 :-: 5 ':&&: OGT 4) @?= EEQP 5
      , simplifyC @(3 :-: 5 ':&&: OLT 4) @?= EEQP 3
      , simplifyC @(OLT 4 ':&&: 3 :-: 5) @?= EEQP 3
      , simplifyC @(3 :-: 7 ':&&: OLT 1) @?= EFalseP
      , simplifyC @(3 :-: 7 ':||: OLT 1) @?= EOrP (EBetweenP 3 7) (ELTP 1)
      , simplifyC @(OGT 7 ':&&: OLT 1) @?= EFalseP
     ] ++
     [ testCase "op1" $
          opBool (evalE (Just 4) (EBetweenP 4 11 `EAndP` EGTP 7 `EAndP` EElemP (9:|[10])))
          @?= Just False

      , testCase "EBetween" $
          simplifyAll (EGTP 4 `EAndP` EBetweenP 3 5)
           @?= Fix (EBetween 5 5) :| [Fix (EEQ 5)]

      , testCase "EBetween" $
           simplifyAll (EBetweenP 4 11 `EAndP` EGTP 7 `EAndP` EElemP (12:|[10,11]))
            @?= Fix (EBetween 10 11) :| []

      , testCase "EBetween" $
            simplifyE ((EGTP 4 `EAndP` EBetweenP 3 5) `EAndP` ELTP 20)
              @?= Fix (EEQ 5)

      , testCase "foldFix" $
               foldFix simplifyAlg (EBetweenP 4 10 `EAndP` ELTP 7)
           @?= Fix (EBetween 4 6)

      , testCase "foldFix" $
               foldFix simplifyAlg (EBetweenP 4 10 `EOrP` ELTP 7)
           @?= Fix (ELE 10)

      , testCase "foldFix" $
               foldFix simplifyAlg (EBetweenP 4 10 `EOrP` ELTP 7)
           @?= Fix (ELE 10)

      , testCase "foldFix" $
               foldFix simplifyAlg (EBetweenP 4 10 `EOrP` ELTP 11)
           @?= Fix (ELE 10)

      , testCase "foldFix" $
               foldFix simplifyAlg (EBetweenP 4 10 `EOrP` ELTP 12)
           @?= Fix (ELE 11)

      , testCase "foldFix" $
               foldFix simplifyAlg (EBetweenP 4 10 `EOrP` EBetweenP 1 2)
           @?= Fix (EOr (Fix (EBetween 4 10)) (Fix (EBetween 1 2)))

      , testCase "foldFix" $
               foldFix simplifyAlg (EBetweenP 4 10 `EOrP` EBetweenP 1 3)
           @?= Fix (EBetween 1 10)

      , testCase "foldFix" $
               foldFix simplifyAlg (EBetweenP 4 10 `EOrP` EBetweenP 1 6)
           @?= Fix (EBetween 1 10)

      , testCase "foldFix" $
               foldFix simplifyAlg (EBetweenP 4 10 `EOrP` EBetweenP 1 2)
           @?= Fix (EOr (Fix (EBetween 4 10)) (Fix (EBetween 1 2)))

      , testCase "foldFix" $
               foldFix simplifyAlg (EBetweenP 4 10 `EOrP` EBetweenP 1 3)
           @?= Fix (EBetween 1 10)

      , testCase "foldFix" $
               foldFix simplifyAlg (ELEP 21 `EOrP` EGEP 17)
           @?= Fix (EOr (Fix (ELE 21)) (Fix (EGE 17)))

      , testCase "foldFix" $
               foldFix simplifyAlg (ELTP 21 `EAndP` EGTP 17)
           @?= Fix (EBetween 18 20)

      , testCase "foldFix" $
               foldFix simplifyAlg (EGTP 4 `EAndP` EBetweenP 3 5)
           @?= Fix (EBetween 5 5)

      , testCase "foldFix" $
               foldFix simplifyAlg (foldFix simplifyAlg (EGTP 4 `EAndP` EBetweenP 3 5))
           @?= Fix (EEQ 5)

      , testCase "foldFix" $
          foldFix (evalAlg Nothing) (ENotP (EElemP (1:|[2..10])) :|| EGTP 12)
            @?= OpRet {opBool = Nothing, opExpr = "(not (rc `elem` [1,2,3,4,5,6,7,8,9,10]) || rc > 12)", opFailures = []}

      , testCase "foldFix" $
          foldFix (evalAlg Nothing) (foldFix simplifyAlg (ENotP (EElemP (1:|[2..10])) :|| EGTP 12))
             @?= OpRet {opBool = Nothing, opExpr = "(not (1 <= rc <= 10) || rc > 12)", opFailures = []}

      ]
      ++ [testCase "assertSimplify" $ assertSimplify 1_000]
      ++ [ adj' 4 10_000 10 $ TQ.testProperty "simplifyResult" simplifyResult
         ]

adj' :: Int -> Int -> Int -> TestTree -> TestTree
adj' sz n ratio =
  adjustOption (const $ TQ.QuickCheckMaxSize sz)
    . adjustOption (max $ TQ.QuickCheckTests n)
    . adjustOption (max $ TQ.QuickCheckMaxRatio ratio)

testOks :: forall (op :: Op). ExprC op => [Maybe Int] -> Assertion
testOks = mapM_ (testOk @op)

testOk :: forall (op :: Op). ExprC op => Maybe Int -> Assertion
testOk = testOpImpl @op True

testFail :: forall (op :: Op). ExprC op => Maybe Int -> Assertion
testFail = testOpImpl @op False

testFails :: forall (op :: Op). ExprC op => [Maybe Int] -> Assertion
testFails = mapM_ (testFail @op)

testOpImpl ::
  forall (op :: Op).
  ExprC op =>
  Bool ->
  Maybe Int ->
  Assertion
testOpImpl b mi =
  let op = evalC @op mi
   in assertBool (opPretty True op) (opBool op == Just b)
