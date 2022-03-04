{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module TestTableFrames where

import Control.Lens ((|>))
import Data.Text (Text)
import Data.Vinyl
import Frames (Frame (..))
import qualified Frames as F
import Frames.CSV
import qualified GHC.Generics as G
import qualified Generics.SOP as GS
import HSql.Core
import HSql.Core.TablePrinter
import HSql.Core.VinylUtils
import qualified Pipes as P

-- use csv reader from Frames and then manipulate that!
F.tableTypes "User" "MOCK_DATA_SMALL.csv"

$(F.declareColumn "fred1" ''Int)

-- type Fred1 = "fred1" :-> Int :: (Symbol, *)
$(F.declareColumn "Fred2" ''Int) -- this works but cos uppercase first letter we cant use # label
-- type Fred2 = "Fred2" :-> Int :: (Symbol, *)

userStream :: F.MonadSafe m => P.Producer User m ()
userStream = readTableOpt userParser "MOCK_DATA_SMALL.csv"

loadUsers :: IO (Frame User)
loadUsers = F.inCoreAoS userStream

testCsvPrint :: IO ()
testCsvPrint = loadUsers >>= wprint

newtype XT2 = XT2 {xt22 :: Int} deriving stock (G.Generic, Show)
instance GS.Generic XT2
instance GS.HasDatatypeInfo XT2
instance DefDec (Dec XT2) where
  defDec = XT2 <$> defDec

newtype XT3 = XT3 {xt33 :: Bool} deriving stock (G.Generic, Show)
instance GS.Generic XT3
instance GS.HasDatatypeInfo XT3
instance DefDec (Dec XT3) where
  defDec = XT3 <$> defDec

data XT4 = XT4 {xt44 :: !Char, xt444 :: !Double} deriving stock (G.Generic, Show)
instance GS.Generic XT4
instance GS.HasDatatypeInfo XT4
instance DefDec (Dec XT4) where
  defDec = XT4 <$> defDec <*> defDec

data T1 = T1 {ta1 :: !String, ta2 :: !Bool, ta3 :: !String, ta4 :: !Int, ta5 :: !Char, ta6 :: !(Maybe Double)} deriving stock (G.Generic, Show)
instance GS.Generic T1
instance GS.HasDatatypeInfo T1

type TF0 = F '["aa" ::: String]

tf00 :: Rec RState '[Sel TF0]
tf00 = RState defDec (Sel [#aa =: "afield" :& RNil, #aa =: "this" :& RNil, #aa =: "world" :& RNil] []) :& RNil

type TF1 = F '["aa" ::: String, "bb" ::: Int]

tf11 :: Rec RState '[Sel TF1]
tf11 = RState defDec (Sel [#aa =: "afield" :& #bb =: 999 :& RNil] []) :& RNil

type TF2 = F '["aa" ::: (Int, String), "bb" ::: (String, String, String)]

tf11x :: Rec RState '[Sel TF2]
tf11x = RState defDec (Sel [#aa =: (44, "afield") :& #bb =: ("a", "b", "c") :& RNil] []) :& RNil

tf22 :: Rec RState '[Sel (String, Int)]
tf22 = RState defDec (Sel [("afield", 999)] []) :& RNil

tf33 :: Rec RState '[Sel (MakeF (String, Int))]
tf33 = RState defDec (Sel [#c1 =: "afield" :& #c2 =: 999 :& RNil, #c1 =: "afield2" :& #c2 =: 1000 :& RNil] []) :& RNil

tf11a :: Rec RState '[Sel TF1]
tf11a = RState defDec (Sel [#aa =: "afield" :& #bb =: 999 :& RNil, #aa =: "fred" :& #bb =: (-12) :& RNil] []) :& RNil

tf11b :: Rec RState '[SelRow TF1]
tf11b = RState defDec (SelRow (#aa =: "afield" :& #bb =: 999 :& RNil) []) :& RNil

tf22b :: Rec RState '[SelRow (String, Int)]
tf22b = RState defDec (SelRow ("afield", 999) []) :& RNil

tf33b :: Rec RState '[SelRow (MakeF (String, Int))]
tf33b = RState defDec (SelRow (#c1 =: "afield" :& #c2 =: 999 :& RNil) []) :& RNil

data IP a = IP {_octet1 :: !a, _octet2 :: !a, _octet3 :: !a, _octet4 :: !a} deriving stock (Show, Eq)

instance Show a => FromCell (IP a) where
  fromCell o iss lr = fromCell o (iss |> (1, "IP")) lr . show
