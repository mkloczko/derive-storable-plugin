{-# OPTIONS_GHC -fplugin Foreign.Storable.Generic.Plugin #-}
{-# LANGUAGE FlexibleInstances #-}
module Instances where

import Types
import Foreign.Storable.Generic

instance (GStorable a, GStorable b) => GStorable (TFlat    a b)
instance (GStorable a, GStorable b) => GStorable (TNested  a b)
instance (GStorable a, GStorable b) => GStorable (TNested2 a b)
