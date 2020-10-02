-- {-# OPTIONS -Wall -Wcompat -Wincomplete-record-updates -Wincomplete-uni-patterns -Wredundant-constraints #-}
{-# OPTIONS -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
module DeferredOrphans where
import Control.DeepSeq
import GHC.Generics (Generic1)
import Data.Vinyl
import qualified Data.Vinyl.CoRec as VC
import HSql.Core.Sql

-- defined in Frames [Frames.ExtraInstances]
--deriving instance Generic1 V.Identity
--instance NFData1 V.Identity

-- we need this else invalidNested1 fails [handled by Frames]
--instance NFData a => NFData (V.Identity a)

deriving instance Generic1 Alle
instance NFData1 Alle

deriving instance Generic1 (Some rev n)
instance NFData1 (Some rev n)

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
    => NFData (RState a) where
  rnf (RState a b c d) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d

instance NFData (VC.CoRec fa '[]) where
  rnf _ = ()

instance (VC.FoldRec xs xs, RecApplicative xs, NFData (fa x), NFData (VC.CoRec fa xs)) => NFData (VC.CoRec fa (x ': xs)) where
   rnf v = case VC.restrictCoRec v of
             Left xs -> rnf xs
             Right x -> rnf x

-- handled by Frames!
--instance (RFoldMap rs, RMap rs, RecAll f rs NFData) => NFData (Rec f rs) where
--  rnf w = rfoldMap V.getConst $ rmap (\(V.Compose (V.Dict x)) -> V.Const $ rnf x) $ VR.reifyConstraint (Proxy @NFData) w

