{-# OPTIONS -Wall -Wcompat -Wincomplete-record-updates -Wincomplete-uni-patterns -Wredundant-constraints #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
module DeferredOrphans where
import Control.DeepSeq
import GHC.Generics (Generic, Generic1)
import Database.HDBC
import Data.Vinyl
import qualified Data.Vinyl.CoRec as VC
import Sql

-- defined in Frames [Frames.ExtraInstances]
--deriving instance Generic1 V.Identity
--instance NFData1 V.Identity

-- we need this else invalidNested1 fails [handled by Frames]
--instance NFData a => NFData (V.Identity a)

instance NFData ConvE
instance NFData UpdNE
instance NFData DecodingE
instance NFData SingleColE
instance NFData UnexpectedResultSetTypeE
instance NFData UnconsumedColE
instance NFData BadE
instance NFData NoResultSetE

instance NFData a => NFData (Enc a)
instance NFData a => NFData (Dec a)
instance NFData a => NFData (Sel a)
instance NFData a => NFData (SelOne a)
instance NFData Upd
instance NFData (UpdN op val)
instance NFData a => NFData (Alle a)

deriving instance Generic1 Alle
instance NFData1 Alle

instance NFData a => NFData (Some rev n a)

deriving instance Generic1 (Some rev n)
instance NFData1 (Some rev n)

instance (NFData a, NFData b) => NFData (a :+: b)

instance NFData (SingleIn (UpdN op val)) where
  rnf UpdNP = ()
instance NFData (SingleIn Upd) where
  rnf UpdP = ()
instance NFData (SingleIn (SelOne a)) where
  rnf (SelOneP d) = d `seq` ()
instance NFData (SingleIn (Sel a)) where
  rnf (SelP d) = d `seq` ()
instance NFData (SingleIn a) => NFData (SingleIn (Alle a)) where
  rnf (AlleP a) = rnf a `seq` ()
instance (NFData (SingleIn a), NFData (SingleIn b)) => NFData (SingleIn (a :+: b)) where
  rnf (a :+: b) = rnf a `seq` rnf b
instance NFData (SingleIn a) => NFData (SingleIn (Some rev n a)) where
  rnf (SomeP a) = rnf a `seq` ()
instance NFData (SingleIn SelRaw) where
  rnf SelRawP = ()

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

