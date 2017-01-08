{-# OPTIONS_GHC -fplugin Foreign.Storable.Generic.Plugin #-}

module Instances where

import Types
import Foreign.Storable.Generic

instance (GStorable a, GStorable b) => GStorable (NFlat    a b)
instance (GStorable a, GStorable b) => GStorable (NNested  a b)
instance (GStorable a, GStorable b) => GStorable (NNested2 a b)
