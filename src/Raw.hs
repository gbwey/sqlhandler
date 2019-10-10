{-# LANGUAGE DeriveGeneric #-}
module Raw where
import Database.HDBC (SqlValue)
import qualified GHC.Generics as G
import Generics.SOP

newtype Raw = Raw { unRaw :: [SqlValue] } deriving (Show,G.Generic)
instance Generic Raw
instance HasDatatypeInfo Raw
