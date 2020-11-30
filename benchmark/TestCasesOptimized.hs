{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DerivingStrategies, DerivingVia, CPP #-}
{-# OPTIONS_GHC -fplugin Foreign.Storable.Generic.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt=Foreign.Storable.Generic.Plugin:-crash #-} 
{-# OPTIONS_GHC -fplugin-opt=Foreign.Storable.Generic.Plugin:-v2 #-} 
module TestCasesOptimized where 

import GHC.Generics (Generic)
import Foreign.Storable.Generic
import Data.Int
import Control.DeepSeq


data C1O = C1O Int32                 deriving (Show, Generic, NFData)
                                     deriving (Storable) via Generically C1O
data C2O = C2O Int8 Int32 Int16      deriving (Show, Generic, NFData)
                                     deriving (Storable) via Generically C2O
data C3O = C3O C2O Int64 C1O         deriving (Show, Generic, NFData)
                                     deriving (Storable) via Generically C3O
data C4O = C4O Double Int8 C3O       deriving (Show, Generic, NFData)
                                     deriving (Storable) via Generically C4O
data C5O = C5O Int32 C2O C4O         deriving (Show, Generic, NFData)
                                     deriving (Storable) via Generically C5O
c1o_def = C1O 3
c2o_def = C2O 3 10 8
c3o_def = C3O c2o_def 11000 c1o_def
c4o_def = C4O 0.312 3 c3o_def 
c5o_def = C5O 100 c2o_def c4o_def 

#ifdef GSTORABLE_SUMTYPES

data S1O = S1Oa Int32 | S1Ob Int16   deriving (Show, Generic, GStorable, NFData)
data S2O = S2Oa Int32 Int64 Int8 | S2Ob S1O Int8 Double
         | S2Oc Int32 Int8 Int16 Int8 | S2Od S1O S1O
         deriving (Show, Generic, GStorable, NFData)

s1o_def = S1Oa 54
s2o_def = S2Ob s1o_def 32 1.5

#endif
