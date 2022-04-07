{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : HSql.Core.Operator
Description : dsl for sql return codes: see 'HSql.Core.Operator.UpdN'
Copyright   : (c) Grant Weyburne, 2021
License     : BSD-3
-}
module HSql.Core.Operator where

import Control.Applicative
import Data.Bool
import Data.Coerce
import Data.Containers.ListUtils (nubOrd)
import Data.Eq.Deriving (deriveEq1)
import Data.Fix
import Data.Function
import Data.Kind
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as N
import qualified Data.List.NonEmpty.Extra as NE
import Data.Maybe
import Data.Proxy (Proxy (Proxy))
import DocUtils.Doc
import GHC.Generics (Generic)
import GHC.TypeLits
import Primus.NonEmpty
import qualified Primus.TypeLevel as TP (Cons1T, pnat)
import Text.Show.Deriving (deriveShow1)

-- | convert a type level nonempty list of signed to nonempty list of ints
type SignedToInts :: NonEmpty Signed -> Constraint
class SignedToInts xs where
  getSignedToInts :: NonEmpty Int

instance SignedC n => SignedToInts (n ':| '[]) where
  getSignedToInts = pure (toSign @n)
instance (SignedC n, SignedToInts (n' ':| ns)) => SignedToInts (n ':| n' ': ns) where
  getSignedToInts = toSign @n N.<| getSignedToInts @(n' ':| ns)

-- | creates a signed positive nonempty list
type PosList :: NonEmpty Nat -> NonEmpty Signed
type family PosList ns = result where
  PosList (n ':| '[]) = 'SPos n ':| '[]
  PosList (n ':| n1 ': ns) = TP.Cons1T ( 'SPos n) (PosList (n1 ':| ns))

-- | creates a signed negative nonempty list
type NegList :: NonEmpty Nat -> NonEmpty Signed
type family NegList ns = result where
  NegList (n ':| '[]) = 'SNeg n ':| '[]
  NegList (n ':| n1 ': ns) = TP.Cons1T ( 'SNeg n) (NegList (n1 ':| ns))

-- | signed version of 'Nat'
data Signed = SNeg !Nat | SPos !Nat

-- | returns the signed value for 'Signed'
type SignedC :: Signed -> Constraint
class SignedC s where
  toSign :: Int

instance KnownNat n => SignedC ( 'SNeg n) where
  toSign = negate (TP.pnat @n)
instance KnownNat n => SignedC ( 'SPos n) where
  toSign = TP.pnat @n

{- $setup
 >>> :m + Control.Lens
-}

-- | list of expressions for the dsl
data ExprF a
  = EBool !Bool
  | ELT !Int
  | ELE !Int
  | EEQ !Int
  | EGE !Int
  | EGT !Int
  | ENE !Int
  | EBetween !Int !Int
  | EElem !(NonEmpty Int)
  | EGroup !String !a
  | ENot !a
  | EAnd !a !a
  | EOr !a !a
  | ESame !a !a
  | EXor !a !a
  deriving stock (Functor, Generic, Show, Eq, Ord)

deriveEq1 ''ExprF
deriveShow1 ''ExprF

-- synonyms for positive values which are the most commonly used

-- | less than or equal for positive values
type OLE (n :: Nat) = 'OLE ( 'SPos n)

-- | less than or equal for positive values
type OLT (n :: Nat) = 'OLT ( 'SPos n)

-- | equal for positive values
type OEQ (n :: Nat) = 'OEQ ( 'SPos n)

-- | greater than for positive values
type OGT (n :: Nat) = 'OGT ( 'SPos n)

-- | greater than or equal for positive values
type OGE (n :: Nat) = 'OGE ( 'SPos n)

-- | not equal for positive values
type ONE (n :: Nat) = 'ONE ( 'SPos n)

-- | between "m" and "n' for positive values
type (:-:) :: Nat -> Nat -> Op
type m :-: n = 'SPos m ':-: 'SPos n

-- | 'elem' for a list of positive numbers
type OElem (ns :: NonEmpty Nat) = 'OElem (PosList ns)

-- | return code string
_RetCodeString :: String
_RetCodeString = "rc"

-- | type synonym the fix point of an expression
type Expr = Fix ExprF

-- | pretty print an expression given an optional initial value
prettyE :: Maybe Int -> Expr -> String
prettyE = foldFix . prettyAlg

{- | evaluate the expression

 >>> opBool (evalE (Just 4) (EBetweenP 4 11 `EAndP` EGTP 7 `EAndP` EElemP (9:|[10])))
 Just False
-}

-- | evaluate an expression given an optional initial value
evalE :: Maybe Int -> Expr -> OpRet
evalE = foldFix . evalAlg

-- | evaluate an expression at the type level given an optional initial value
evalC :: forall op. ExprC op => Maybe Int -> OpRet
evalC mi = foldFix (evalAlg mi) (toExpr @op)

-- | unwrap a 'Fix' value
unFix :: Fix f -> f (Fix f)
unFix = coerce

{- | repeatedly simplify until no change returning all the results

 >>> simplifyAll (EGTP 4 `EAndP` EBetweenP 3 5)
 Fix (EBetween 5 5) :| [Fix (EEQ 5)]

 >>> simplifyAll (EBetweenP 4 11 `EAndP` EGTP 7 `EAndP` EElemP (12:|[10,11]))
 Fix (EBetween 10 11) :| []
-}
simplifyAll :: Expr -> NonEmpty Expr
simplifyAll = go . foldFix simplifyAlg
 where
  go !e =
    let e' = foldFix simplifyAlg e
     in if e == e'
          then pure e
          else e N.<| go e'

-- | repeatedly simplify a type level expression until no change returning all the results
simplifyC :: forall op. ExprC op => Expr
simplifyC = simplifyE (toExpr @op)

{- | repeatedly simplifyAlg until no change returning all the results

 >>> simplifyE ((EGTP 4 `EAndP` EBetweenP 3 5) `EAndP` ELTP 20)
 Fix (EEQ 5)
-}
simplifyE :: Expr -> Expr
simplifyE = N.last . simplifyAll

-- | simplify an expression at the type level using an optional initial value
simplifyEvalC :: forall op. ExprC op => Maybe Int -> OpRet
simplifyEvalC mi = foldFix (evalAlg mi) $ simplifyC @op

{- | simplifyAlg one layer of the expression

 >>> foldFix simplifyAlg (EBetweenP 4 10 `EAndP` ELTP 7)
 Fix (EBetween 4 6)

 >>> foldFix simplifyAlg (EBetweenP 4 10 `EOrP` ELTP 7)
 Fix (ELE 10)

 >>> foldFix simplifyAlg (EBetweenP 4 10 `EOrP` ELTP 7)
 Fix (ELE 10)

 >>> foldFix simplifyAlg (EBetweenP 4 10 `EOrP` ELTP 11)
 Fix (ELE 10)

 >>> foldFix simplifyAlg (EBetweenP 4 10 `EOrP` ELTP 12)
 Fix (ELE 11)

 >>> foldFix simplifyAlg (EBetweenP 4 10 `EOrP` EBetweenP 1 2)
 Fix (EOr (Fix (EBetween 4 10)) (Fix (EBetween 1 2)))

 >>> foldFix simplifyAlg (EBetweenP 4 10 `EOrP` EBetweenP 1 3)
 Fix (EBetween 1 10)

 >>> foldFix simplifyAlg (EBetweenP 4 10 `EOrP` EBetweenP 1 6)
 Fix (EBetween 1 10)

 >>> foldFix simplifyAlg (EBetweenP 4 10 `EOrP` EBetweenP 1 2)
 Fix (EOr (Fix (EBetween 4 10)) (Fix (EBetween 1 2)))

 >>> foldFix simplifyAlg (EBetweenP 4 10 `EOrP` EBetweenP 1 3)
 Fix (EBetween 1 10)

 >>> foldFix simplifyAlg (ELEP 21 `EOrP` EGEP 17)
 Fix (EOr (Fix (ELE 21)) (Fix (EGE 17)))

 >>> foldFix simplifyAlg (ELTP 21 `EAndP` EGTP 17)
 Fix (EBetween 18 20)

 >>> foldFix simplifyAlg (EGTP 4 `EAndP` EBetweenP 3 5)
 Fix (EBetween 5 5)

 >>> foldFix simplifyAlg (foldFix simplifyAlg (EGTP 4 `EAndP` EBetweenP 3 5))
 Fix (EEQ 5)
-}
simplifyAlg :: ExprF Expr -> Expr
-- simplifyAlg :: Algebra ExprF Expr
simplifyAlg = \case
  ENot (ELEP i) -> EGTP i
  ENot (ELTP i) -> EGEP i
  ENot (EEQP i) -> ENEP i
  ENot (EGTP i) -> ELEP i
  ENot (EGEP i) -> ELTP i
  ENot (ENEP i) -> EEQP i
  ENot (EBoolP b) -> EBoolP (not b)
  ENot (ENotP e) -> e
  (EGroup _ x) -> x -- strip group so we can simplifyAlg further
  e@(EAnd x y) ->
    case curry toBB2 <$> toBB x <*> toBB y of
      Just w -> fromBB $ fromBB2And w
      Nothing -> fromMaybe (Fix e) (doAnd (x, y) <|> doAnd (y, x))
  e@(EOr x y) ->
    case curry toBB2 <$> toBB x <*> toBB y of
      Just w -> case fromBB <$> fromBB2Or w of
        Just r -> r
        Nothing -> Fix e
      Nothing -> fromMaybe (Fix e) (doOr (x, y) <|> doOr (y, x))
  ESame (EBoolP True) x -> x
  ESame x (EBoolP True) -> x -- flipped
  ESame (EBoolP False) x -> ENotP x
  ESame x (EBoolP False) -> ENotP x -- flipped
  EXor (EBoolP True) x -> ENotP x
  EXor x (EBoolP True) -> ENotP x -- flipped
  EXor (EBoolP False) x -> x
  EXor x (EBoolP False) -> x -- flipped
  EBetween i j
    | i > j -> EFalseP
    | i == j -> EEQP i
  EElem (N.sort . NE.nubOrd -> ns) ->
    if isSequence1 ns
      then EBetweenP (N.head ns) (N.last ns)
      else EElemP ns
  e -> Fix e

-- | pattern synonym for lifting a 'Bool' into a 'Expr'
pattern EBoolP :: Bool -> Expr
pattern EBoolP b = Fix (EBool b)

-- | pattern synonym for taking the "not" of an expression
pattern ENotP :: Expr -> Expr
pattern ENotP a = Fix (ENot a)

-- | pattern synonym for lifting a nonempty list of ints into 'EElem'
pattern EElemP :: NonEmpty Int -> Expr
pattern EElemP ns = Fix (EElem ns)

-- | pattern synonym for lifting an int into a 'ELE'
pattern ELEP :: Int -> Expr
pattern ELEP i = Fix (ELE i)

-- | pattern synonym for lifting an int into 'ELT'
pattern ELTP :: Int -> Expr
pattern ELTP i = Fix (ELT i)

-- | pattern synonym for lifting an int into 'EEQ'
pattern EEQP :: Int -> Expr
pattern EEQP i = Fix (EEQ i)

-- | pattern synonym for lifting an int into 'EGT'
pattern EGTP :: Int -> Expr
pattern EGTP i = Fix (EGT i)

-- | pattern synonym for lifting an int into 'EGE'
pattern EGEP :: Int -> Expr
pattern EGEP i = Fix (EGE i)

-- | pattern synonym for lifting an int into 'ENE'
pattern ENEP :: Int -> Expr
pattern ENEP i = Fix (ENE i)

-- | pattern synonym for 'EAnd' two expressions
pattern EAndP :: Expr -> Expr -> Expr
pattern EAndP a b = Fix (EAnd a b)

-- | pattern synonym for 'EAnd' two expressions using an operator
pattern (:&&) :: Expr -> Expr -> Expr
pattern (:&&) a b = Fix (EAnd a b)

-- | pattern synonym for 'EOr' two expressions
pattern EOrP :: Expr -> Expr -> Expr
pattern EOrP a b = Fix (EOr a b)

-- | pattern synonym for 'EOr' two expressions using an operator
pattern (:||) :: Expr -> Expr -> Expr
pattern (:||) a b = Fix (EOr a b)

-- | pattern synonym for 'ESame' two expressions
pattern ESameP :: Expr -> Expr -> Expr
pattern ESameP a b = Fix (ESame a b)

-- | pattern synonym for 'ESame' two expressions using an operator
pattern (:==) :: Expr -> Expr -> Expr
pattern (:==) a b = Fix (ESame a b)

-- | pattern synonym for 'EXor' two expressions
pattern EXorP :: Expr -> Expr -> Expr
pattern EXorP a b = Fix (EXor a b)

-- | pattern synonym for 'EXor' two expressions using an operator
pattern (:/=) :: Expr -> Expr -> Expr
pattern (:/=) a b = Fix (EXor a b)

infixl 3 :&&, :==, :/=
infixl 2 :||

-- | pattern synonym for a False expression value
pattern EFalseP :: Expr
pattern EFalseP = Fix (EBool False)

-- | pattern synonym for a True expression value
pattern ETrueP :: Expr
pattern ETrueP = Fix (EBool True)

-- | pattern synonym for 'EGroup'
pattern EGroupP :: String -> Expr -> Expr
pattern EGroupP s e = Fix (EGroup s e)

-- | pattern synonym for 'EGroup' using an operator
pattern (:!) :: String -> Expr -> Expr
pattern (:!) s e = Fix (EGroup s e)

infixl 4 :!

-- | pattern synonym with two int values for 'EBetween'
pattern EBetweenP :: Int -> Int -> Expr
pattern EBetweenP i j = Fix (EBetween i j)

-- | pattern synonym with two int values for 'EBetween' using an operator
pattern (:-) :: Int -> Int -> Expr
pattern (:-) i j = Fix (EBetween i j)

infixl 4 :-

{- | algebra for evaluating an expression

 >>> foldFix (evalAlg Nothing) (ENotP (EElemP (1:|[2..10])) :|| EGTP 12)
 OpRet {opBool = Nothing, opExpr = "(not (rc `elem` [1,2,3,4,5,6,7,8,9,10]) || rc > 12)", opFailures = []}

 >>> foldFix (evalAlg Nothing) $ foldFix simplifyAlg (ENotP (EElemP (1:|[2..10])) :|| EGTP 12)
 OpRet {opBool = Nothing, opExpr = "(not (1 <= rc <= 10) || rc > 12)", opFailures = []}
-}
evalAlg :: Maybe Int -> ExprF OpRet -> OpRet
evalAlg mi = \case
  EBool b -> exprOp0 b
  ELT j -> exprNat1 "<" (< j) j mi
  ELE j -> exprNat1 "<=" (<= j) j mi
  EEQ j -> exprNat1 "==" (== j) j mi
  EGE j -> exprNat1 ">=" (>= j) j mi
  EGT j -> exprNat1 ">" (> j) j mi
  ENE j -> exprNat1 "/=" (/= j) j mi
  EBetween j k -> exprBetween j k mi
  EElem ns -> exprOpElem ns mi
  EGroup s e -> exprOpGroup s e
  ENot e -> exprOpNot e
  EAnd e e' -> exprOp2 e e' "&&" (&&)
  EOr e e' -> exprOp2 e e' "||" (||)
  ESame e e' -> exprOp2 e e' "==" (==)
  EXor e e' -> exprOp2 e e' "/=" (/=)

-- already handled in evalAlg

-- | pretty print an expression using an optional initial value
prettyAlg :: Maybe Int -> ExprF String -> String
prettyAlg mi = \case
  EBool b -> show b
  ELT j -> one "<" j
  ELE j -> one "<=" j
  EEQ j -> one "==" j
  EGE j -> one ">=" j
  EGT j -> one ">" j
  ENE j -> one "/=" j
  EBetween j k -> show j ++ " <= " ++ rc ++ " <= " ++ show k
  EElem ns -> rc ++ " `elem` " ++ show (N.toList ns)
  EGroup s e -> wrapBraces s ++ addParens e
  ENot e -> "not " ++ addParens e
  EAnd e e' -> two e e' "&&"
  EOr e e' -> two e e' "||"
  ESame e e' -> two e e' "=="
  EXor e e' -> two e e' "/="
 where
  rc = maybe _RetCodeString show mi
  one opstring j = rc ++ " " ++ opstring ++ " " ++ show j
  two e e' opstring = bool wrapParens id (on (&&) hasParens e e') (e ++ " " ++ opstring ++ " " ++ e')

-- | handler for a single int-valued function
exprNat1 ::
  String ->
  (Int -> Bool) ->
  Int ->
  Maybe Int ->
  OpRet
exprNat1 opstring f j mi =
  exprInit
    (f <$> mi)
    (maybe _RetCodeString show mi ++ " " ++ opstring ++ " " ++ show j)

-- | handler for a between expression
exprBetween ::
  Int ->
  Int ->
  Maybe Int ->
  OpRet
exprBetween j k =
  \case
    Nothing -> OpRet Nothing (show j ++ " <= " ++ _RetCodeString ++ " <= " ++ show k) []
    Just i ->
      let b1 = i >= j
          b2 = i <= k
          c1 = [show i ++ " >= " ++ show j | not b1]
          c2 = [show i ++ " <= " ++ show k | not b2]
       in OpRet
            { opBool = Just (b1 && b2)
            , opExpr = show j ++ " <= " ++ show i ++ " <= " ++ show k
            , opFailures = c1 ++ c2
            }

-- | handler for the inital value
exprInit :: Maybe Bool -> String -> OpRet
exprInit mb expr =
  OpRet
    { opBool = mb
    , opExpr = expr
    , opFailures = [expr | mb == Just False]
    }

-- | handler for a "not" expression
exprOpNot ::
  OpRet ->
  OpRet
exprOpNot op =
  let expr = "not " ++ addParens (opExpr op)
      mb = not <$> opBool op
   in OpRet
        { opExpr = expr
        , opBool = mb
        , opFailures = opFailures op <> [expr | mb == Just False]
        }

-- | handler for a "elem" expression
exprOpElem ::
  NonEmpty Int ->
  Maybe Int ->
  OpRet
exprOpElem js mi =
  exprInit
    ((`elem` js) <$> mi)
    (maybe _RetCodeString show mi ++ " `elem` " ++ show (N.toList js))

-- | handler for grouping an expression
exprOpGroup ::
  String ->
  OpRet ->
  OpRet
exprOpGroup s op =
  op{opExpr = wrapBraces s ++ addParens (opExpr op)}

-- | handler for a boolean binary function
exprOp2 ::
  OpRet ->
  OpRet ->
  String ->
  (Bool -> Bool -> Bool) ->
  OpRet
exprOp2 op op' opstring f =
  OpRet
    { opExpr = bool wrapParens id (on (&&) (hasParens . opExpr) op op') (opExpr op ++ " " ++ opstring ++ " " ++ opExpr op')
    , opBool = f <$> opBool op <*> opBool op'
    , opFailures = opFailures op <> opFailures op'
    }

-- | handler for boolean
exprOp0 ::
  Bool ->
  OpRet
exprOp0 b = exprInit (Just b) (show b)

-- | take a type level expression to the runtime
type ExprC :: Op -> Constraint
class ExprC a where
  toExpr :: Expr

instance ExprC 'OFalse where
  toExpr = EFalseP
instance ExprC 'OTrue where
  toExpr = ETrueP
instance SignedC n => ExprC ( 'OLT n) where
  toExpr = ELTP (toSign @n)
instance SignedC n => ExprC ( 'OLE n) where
  toExpr = ELEP (toSign @n)
instance SignedC n => ExprC ( 'OEQ n) where
  toExpr = EEQP (toSign @n)
instance SignedC n => ExprC ( 'OGE n) where
  toExpr = EGEP (toSign @n)
instance SignedC n => ExprC ( 'OGT n) where
  toExpr = EGTP (toSign @n)
instance SignedC n => ExprC ( 'ONE n) where
  toExpr = ENEP (toSign @n)
instance (SignedC m, SignedC n) => ExprC (m ':-: n) where
  toExpr = EBetweenP (toSign @m) (toSign @n)

instance ExprC a => ExprC ( 'ONot a) where
  toExpr = ENotP (toExpr @a)
instance (KnownSymbol s, ExprC a) => ExprC ( 'OGroup s a) where
  toExpr = EGroupP (symbolVal (Proxy @s)) (toExpr @a)

instance SignedToInts (n ':| ns) => ExprC ( 'OElem (n ':| ns)) where
  toExpr = EElemP (getSignedToInts @(n ':| ns))

instance (ExprC a, ExprC b) => ExprC (a ':&&: b) where
  toExpr = EAndP (toExpr @a) (toExpr @b)
instance (ExprC a, ExprC b) => ExprC (a ':||: b) where
  toExpr = EOrP (toExpr @a) (toExpr @b)
instance (ExprC a, ExprC b) => ExprC (a ':/=: b) where
  toExpr = EXorP (toExpr @a) (toExpr @b)
instance (ExprC a, ExprC b) => ExprC (a ':==: b) where
  toExpr = ESameP (toExpr @a) (toExpr @b)

-- | simple predicates on the return code from a sql update
data Op
  = OFalse
  | OTrue
  | OLT !Signed
  | OLE !Signed
  | OEQ !Signed
  | OGE !Signed
  | OGT !Signed
  | ONE !Signed
  | !Signed :-: !Signed
  | ONot !Op
  | !Op :&&: !Op
  | !Op :||: !Op
  | !Op :/=: !Op
  | !Op :==: !Op
  | OElem !(NonEmpty Signed)
  | OGroup !Symbol !Op
  deriving stock (Generic)

infixl 5 :-:
infixl 3 :&&:, :==:, :/=:
infixl 2 :||:

-- | the state of processing the current expression
data OpRet = OpRet
  { opBool :: !(Maybe Bool)
  , opExpr :: !String
  , opFailures :: ![String]
  }
  deriving stock (Show, Eq, Ord, Generic) -- want ordering based on Bool so make it first

-- | pretty print 'OpRet'
opPretty :: Bool -> OpRet -> String
opPretty verbose op =
  let ex1 = stripParens (opExpr op)
      fails1 = map stripParens (opFailures op)
      failsmsg = case filter (/= ex1) fails1 of
        [] -> mempty
        xs -> " => failures: [ " ++ L.intercalate ", " xs ++ " ]"
   in case opBool op of
        Nothing ->
          if verbose
            then ex1
            else ex1 ++ failsmsg
        Just b
          | ex1 == show b -> ex1
          | verbose ->
              let pref = show b ++ ": " ++ wrap ("[ ", " ]") ex1
               in pref ++ failsmsg
          | otherwise -> show b ++ ": " ++ wrap ("[ ", " ]") ex1

-- | converts 'BB2' back to 'BB' using intersection
fromBB2And :: BB2 -> BB
fromBB2And = \case
  BLELE i j -> BLE (min i j)
  BLEGE i j -> BTW i j -- if invalid will get cleaned up on the next iteration
  BLETW i i' j'
    | i >= i' -> BTW (min i i') (min i j')
    | otherwise -> BTW 1 0 -- ie FalseP
  BGEGE j j' -> BGE (max j j')
  BGETW j i' j'
    | j <= j' -> BTW (max i' j) (max i' j')
    | otherwise -> BTW 1 0 -- ie FalseP
  BTWTW i j i' j' -> BTW (max i i') (min j j')

-- eliminate junk betweens ie EBetween 4 2

-- | converts 'BB2' back to 'BB' using union
fromBB2Or :: BB2 -> Maybe BB
fromBB2Or = \case
  BLELE i j -> Just $ BLE (max i j)
  BLEGE _i _j -> Nothing
  BLETW i i' j'
    | i + 1 < i' -> Nothing
    | otherwise -> Just $ BLE (max i j')
  BGEGE j j' -> Just $ BGE (min j j')
  BGETW j i' j'
    | j > j' + 1 -> Nothing
    | otherwise -> Just $ BGE (min j i')
  BTWTW i j i' j'
    | (i <= i' && j + 1 >= i') || (i' <= i && j' + 1 >= i) -> Just $ BTW (min i i') (max j j') -- not order dependent i<=i'
    --  | j+1 >= i' ->  Just $ BTW (min i i') (max j j') -- order dependent i<=i' in toBB2
    | otherwise -> Nothing

-- | covers all possible cases of pairs of 'BB'
data BB2
  = BLELE !Int !Int
  | BLEGE !Int !Int
  | BLETW !Int !Int !Int
  | BGEGE !Int !Int
  | BGETW !Int !Int !Int
  | BTWTW !Int !Int !Int !Int
  deriving stock (Eq, Ord, Show, Generic)

-- | simplifies combinations of 'BB2'
toBB2 :: (BB, BB) -> BB2
toBB2 = \case
  (BLE i, BLE j) -> BLELE i j
  (BLE i, BGE j) -> BLEGE j i
  (BGE j, BLE i) -> BLEGE j i
  (BLE i, BTW i' j') -> BLETW i i' j'
  (BTW i' j', BLE i) -> BLETW i i' j'
  (BGE i, BGE j) -> BGEGE i j
  (BGE i, BTW i' j') -> BGETW i i' j'
  (BTW i' j', BGE i) -> BGETW i i' j'
  (BTW i j, BTW i' j')
    | i < i' -> BTWTW i j i' j' -- retain ordering
    | otherwise -> BTWTW i' j' i j

-- | simplify comparison expressions
data BB
  = BLE !Int
  | BGE !Int
  | BTW !Int !Int
  deriving stock (Eq, Ord, Show, Generic)

-- | try to convert an expression to 'BB'
toBB :: Expr -> Maybe BB
toBB = \case
  ELEP i -> Just $ BLE i
  ELTP i -> Just $ BLE (i - 1)
  EEQP i -> Just $ BTW i i
  EGTP i -> Just $ BGE (i + 1)
  EGEP i -> Just $ BGE i
  EBetweenP i j | j >= i -> Just $ BTW i j -- only allow valid stuff
  _skip -> Nothing

-- | convert back from 'BB' to an expression
fromBB :: BB -> Expr
fromBB (BLE i) = ELEP i
fromBB (BGE j) = EGEP j
fromBB (BTW i j) = EBetweenP i j

-- | "and" two expressions together
doAnd :: (Expr, Expr) -> Maybe Expr
doAnd = \case
  (EEQP i, ENEP j) | i == j -> Just EFalseP
  (x@(ENEP i), ENEP j) | i == j -> Just x
  (x, y@(EBoolP b)) -> Just $ if b then x else y
  (EElemP is, EElemP js) -> Just $ makeEElem (L.sort $ on L.intersect (nubOrd . N.toList) is js)
  (EElemP ns, y@(EEQP j)) -> Just $ if j `elem` ns then y else EFalseP
  (EElemP ns, ENEP j) -> Just $ makeEElem (L.delete j (nubOrd $ N.toList ns))
  (EElemP is, ENotP (EElemP js)) -> Just $ makeEElem (on (L.\\) (nubOrd . N.toList) is js)
  (ENotP (EElemP is), y@(EEQP i))
    | i `elem` is -> Just EFalseP
    | otherwise -> Just y
  (ENotP (EElemP is), ENEP i) -> Just $ ENotP (sortEElem (i N.<| is))
  _skip -> Nothing

-- | create an 'EElem' expression from a list of ints
makeEElem :: [Int] -> Expr
makeEElem = \case
  [] -> EFalseP
  z : zs -> sortEElem (z :| zs)

-- | sort 'EElem' contents so we can compare
sortEElem :: NonEmpty Int -> Expr
sortEElem = EElemP . N.sort . NE.nubOrd

-- | "or" two expressions together
doOr :: (Expr, Expr) -> Maybe Expr
doOr = \case
  (EEQP i, ENEP j) | i == j -> Just ETrueP
  (x@(ENEP i), ENEP j) | i == j -> Just x
  (EBoolP b, y) -> Just $ if b then ETrueP else y
  (EElemP is, EElemP js) -> Just $ sortEElem (is <> js)
  (EElemP ns, EEQP j) -> Just $ sortEElem $ j N.<| ns
  (EElemP ns, ENEP j) | j `elem` ns -> Just ETrueP
  (EElemP is, ENotP (EElemP js)) ->
    case on L.intersect N.toList is js of
      [] -> Nothing
      ws@(_ : _) -> Just $ EOrP (makeEElem (nubOrd (N.toList is) L.\\ ws)) (ENotP (makeEElem (nubOrd (N.toList js) L.\\ ws)))
  (ENotP (EElemP is), EEQP i) | i `elem` is -> Just $ ENotP $ makeEElem $ L.delete i (nubOrd (N.toList is))
  (ENotP (EElemP is), x@(ENEP i))
    | i `elem` is -> Just x
    | otherwise -> Just ETrueP
  _skip -> Nothing

-- experimental

-- | rewrite rules and simplification for expressions
rewriteRulesAlg :: ExprF Expr -> Expr
rewriteRulesAlg = \case
  ENot (EBetweenP i j) -> ELTP i :|| EGTP j
  ENot (e :&& f) -> ENotP e :|| ENotP f
  ENot (e :|| f) -> ENotP e :&& ENotP f
  EAnd e f | e == f -> e
  EOr e f | e == f -> f
  EAnd e (ENotP f) | e == f -> EFalseP
  EAnd (ENotP f) e | e == f -> EFalseP -- flipped
  EAnd e (ENotP f) | e == f -> ETrueP
  EOr (ENotP f) e | e == f -> ETrueP -- flipped
  ENot (ENotP e) -> e
  EAnd (e :|| e1) (f :|| f1)
    | e == f -> e :|| e1 :&& f1
    | e == f1 -> e :|| e1 :&& f
    | e1 == f -> e1 :|| e :&& f1
    | e1 == f1 -> e1 :|| e :&& f
  e -> Fix e

-- | takes an 'Expr' and flattens it out
toExprZ :: Expr -> ExprZ
toExprZ (Fix e) = case e of
  EBool b -> ZBool b
  ELE i -> ZLE i
  ELT i -> ZLT i
  EEQ i -> ZEQ i
  EGT i -> ZGT i
  EGE i -> ZGE i
  ENE i -> ZNE i
  EBetween i j -> ZBetween i j
  EElem ns -> ZElem ns
  EGroup s a -> ZGroup s (toExprZ a)
  ENot a -> ZNot (toExprZ a)
  EAnd a b -> on ZAnd toExprZ a b
  EOr a b -> on ZOr toExprZ a b
  ESame a b -> on ZSame toExprZ a b
  EXor a b -> on ZXor toExprZ a b

-- | experimental: flattened version of Expr as Expr has Nat/Signed which is not inhabitable
data ExprZ
  = ZBool !Bool
  | ZLT !Int
  | ZLE !Int
  | ZEQ !Int
  | ZGE !Int
  | ZGT !Int
  | ZNE !Int
  | ZBetween !Int !Int
  | ZElem !(NonEmpty Int)
  | ZNot !ExprZ
  | ZAnd !ExprZ !ExprZ
  | ZOr !ExprZ !ExprZ
  | ZSame !ExprZ !ExprZ
  | ZXor !ExprZ !ExprZ
  | ZGroup !String !ExprZ
  deriving stock (Generic, Show, Eq, Ord)
