{-# OPTIONS -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module DeferredOrphans where

import Control.DeepSeq
import Data.Vinyl
import qualified Data.Vinyl.CoRec as VC
import qualified Data.Vinyl.Functor as V
import GHC.Generics (Generic1)
import HSql.Core.Sql

-- defined in Frames [Frames.ExtraInstances] so dont import Frames!!
--    if you import Frames then you will get conflicts between vinyl core and frames
--     for a Rec NFData instance that will break testsqldeferred!
deriving stock instance Generic1 V.Identity
instance NFData1 V.Identity

-- we need this else invalidNested1 fails to fail correctly [handled by Frames]
instance NFData a => NFData (V.Identity a)

deriving stock instance Generic1 Rev
instance NFData1 Rev

deriving stock instance Generic1 Alle
instance NFData1 Alle

deriving stock instance Generic1 Some
instance NFData1 Some

deriving stock instance Generic1 (Range m n)
instance NFData1 (Range m n)

deriving stock instance Generic1 May
instance NFData1 May

deriving stock instance Generic1 (Exact n)
instance NFData1 (Exact n)

instance NFData (SingleIn (UpdN op)) where
  rnf UpdNP = ()
instance NFData (SingleIn Upd) where
  rnf UpdP = ()
instance NFData (SingleIn (SelRow a)) where
  rnf (SelRowP d) = d `seq` ()
instance NFData (SingleIn (Sel a)) where
  rnf (SelP d) = d `seq` ()
instance NFData (SingleIn (SelRowCol a)) where
  rnf (SelRowColP d) = d `seq` ()
instance NFData (SingleIn (SelCol a)) where
  rnf (SelColP d) = d `seq` ()
instance NFData (SingleIn a) => NFData (SingleIn (Rev a)) where
  rnf (RevP a) = rnf a `seq` ()
instance NFData (SingleIn a) => NFData (SingleIn (Alle a)) where
  rnf (AlleP a) = rnf a `seq` ()
instance NFData (SingleIn a) => NFData (SingleIn (Some a)) where
  rnf (SomeP a) = rnf a `seq` ()
instance (NFData (SingleIn a), NFData (SingleIn b)) => NFData (SingleIn (a :+: b)) where
  rnf (a :+: b) = rnf a `seq` rnf b
instance (NFData (SingleIn a), NFData (SingleIn b)) => NFData (SingleIn (a :*: b)) where
  rnf (a :*: b) = rnf a `seq` rnf b
instance NFData (SingleIn a) => NFData (SingleIn (May a)) where
  rnf (MayP a) = rnf a `seq` ()
instance NFData (SingleIn a) => NFData (SingleIn (Range m n a)) where
  rnf (RangeP a) = rnf a `seq` ()
instance NFData (SingleIn a) => NFData (SingleIn (Exact n a)) where
  rnf (ExactP a) = rnf a `seq` ()
instance NFData (SingleIn SelRaw) where
  rnf SelRawP = ()

instance
  (NFData (SingleIn a), NFData a) =>
  NFData (RState a)
  where
  rnf (RState a b) = rnf a `seq` rnf b

instance NFData (VC.CoRec fa '[]) where
  rnf _ = ()

instance (VC.FoldRec xs xs, RecApplicative xs, NFData (fa x), NFData (VC.CoRec fa xs)) => NFData (VC.CoRec fa (x ': xs)) where
  rnf v = case VC.restrictCoRec v of
    Left xs -> rnf xs
    Right x -> rnf x

-- handled by Frames!
-- instance (RFoldMap rs, RMap rs, RecAll f rs NFData) => NFData (Rec f rs) where
--  rnf w = rfoldMap V.getConst $ rmap (\(V.Compose (V.Dict x)) -> V.Const $ rnf x) $ VR.reifyConstraint (Proxy @NFData) w
