{-# OPTIONS_GHC -fplugin Foreign.Storable.Generic.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt=Foreign.Storable.Generic.Plugin:-crash #-} 

module Instances where

import Types
import Foreign.Storable.Generic

instance GStorable NFlat
instance GStorable NNested
instance GStorable NNested2
