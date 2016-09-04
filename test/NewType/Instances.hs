{-# OPTIONS_GHC -fplugin Foreign.Storable.Generic.Plugin #-}

module Instances where

import Types
import Foreign.Storable.Generic

instance GStorable NFlat
instance GStorable NNested
instance GStorable NNested2
