{-# OPTIONS_GHC -fplugin Foreign.Storable.Generic.Plugin #-}

module Instances where

import Types
import Foreign.Storable.Generic

instance GStorable Flat
-- instance GStorable Nested
-- instance GStorable Nested2
