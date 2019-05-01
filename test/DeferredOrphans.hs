{-# OPTIONS -Wall -Wcompat -Wincomplete-record-updates -Wincomplete-uni-patterns -Wredundant-constraints #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
module DeferredOrphans where
import Control.DeepSeq
import GHC.Generics (Generic,Generic1)
import Database.HDBC
--import qualified Data.Vinyl as V
import Data.Vinyl
--import qualified Data.Vinyl.Functor as V
--import Data.Vinyl.TypeLevel hiding (Nat)
--import Data.Proxy
import qualified Data.Vinyl.CoRec as VC
--import qualified Data.Vinyl.Recursive as VR
import Sql
import PredHelper

-- defined in Frames [Frames.ExtraInstances]
--deriving instance Generic1 V.Identity
--instance NFData1 V.Identity

-- dude we need this else invalidNested1 fails [handled by Frames
--instance NFData a => NFData (V.Identity a)

deriving instance Generic ConvE
instance NFData ConvE

deriving instance Generic PredE
instance NFData PredE

deriving instance Generic PredExceptionE
instance NFData PredExceptionE

deriving instance Generic DecodingE
instance NFData DecodingE

deriving instance Generic SingleE
instance NFData SingleE

deriving instance Generic SingleColE
instance NFData SingleColE

deriving instance Generic SelUpdE
instance NFData SelUpdE

deriving instance Generic UnconsumedE
instance NFData UnconsumedE

deriving instance Generic UnconsumedColE
instance NFData UnconsumedColE

deriving instance Generic BadE
instance NFData BadE

deriving instance Generic NoResultSetE
instance NFData NoResultSetE

--deriving instance Generic (Pred a)
--instance NFData a => NFData (Pred a)

--deriving instance Generic (Enc a)
instance NFData a => NFData (Enc a)

--deriving instance Generic (Dec a)
instance NFData a => NFData (Dec a)

--deriving instance Generic (Sel a)
instance NFData a => NFData (Sel a)

--deriving instance Generic (SelOne a)
instance NFData a => NFData (SelOne a)

--deriving instance Generic Upd
instance NFData Upd

--deriving instance Generic (UpdN op val)
instance NFData (UpdN op val)

--deriving instance Generic (Alle a)
instance NFData a => NFData (Alle a)

deriving instance Generic1 Alle
instance NFData1 Alle

--deriving instance Generic (Some n a)
instance NFData a => NFData (Some n a)

deriving instance Generic1 (Some n)
instance NFData1 (Some n)

--deriving instance Generic (a :+: b)
instance (NFData a, NFData b) => NFData (a :+: b)

-- todo: how to get rnf working for Pred a and Dec a: to get it working would need NFData a => in the definition of the GADT X itself for each constructor eg SelP SelOneP etc
-- luckily this is deep enough cos at least goes into Alle and Some and Either but doesnt allow us to look at Pred a / Dec a
instance NFData (SingleIn (UpdN op val)) where
  rnf UpdNP = ()
instance NFData (SingleIn Upd) where
  rnf (UpdP _p) = ()
instance NFData (SingleIn (SelOne a)) where
  rnf (SelOneP p d) = p `seq` d `seq` () -- rnf p `seq` rnf d
instance NFData (SingleIn (Sel a)) where
  rnf (SelP p d) = p `seq` d `seq` () -- rnf p `seq` rnf d
instance NFData (SingleIn a) => NFData (SingleIn (Alle a)) where
  rnf (AlleP a p) = rnf a `seq` p `seq` ()
instance (NFData (SingleIn a), NFData (SingleIn b)) => NFData (SingleIn (a :+: b)) where
  rnf (a :+: b) = rnf a `seq` rnf b
instance NFData (SingleIn a) => NFData (SingleIn (Some n a)) where
  rnf (SomeP a p) = rnf a `seq` p `seq` ()
instance NFData (SingleIn SelRaw) where
  rnf (SelRawP _p) = ()

instance (NFData (SingleIn a), NFData (SingleOut a), NFData a)
    => NFData (ZZZ a) where
  rnf (ZZZ a b c d) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d

instance NFData (VC.CoRec fa '[]) where
  rnf _ = ()

instance (VC.FoldRec xs xs, RecApplicative xs, NFData (fa x), NFData (VC.CoRec fa xs)) => NFData (VC.CoRec fa (x ': xs)) where
   rnf v = case VC.restrictCoRec v of
             Left xs -> rnf xs
             Right x -> rnf x

deriving instance Generic SqlValue
deriving instance Generic SqlColDesc
deriving instance Generic SqlTypeId
deriving instance Generic SqlInterval
instance NFData SqlValue
instance NFData SqlColDesc
instance NFData SqlTypeId
instance NFData SqlInterval

-- handled by Frames!
--instance (RFoldMap rs, RMap rs, RecAll f rs NFData) => NFData (Rec f rs) where
--  rnf w = rfoldMap V.getConst $ rmap (\(V.Compose (V.Dict x)) -> V.Const $ rnf x) $ VR.reifyConstraint (Proxy @NFData) w

