{-# OPTIONS_GHC -fplugin Foreign.Storable.Generic.Plugin #-}
{-# LANGUAGE FlexibleInstances #-}
module Parametrised.Instances where

import Parametrised.Types
import Foreign.Storable.Generic
import Foreign.Ptr 

instance (GStorable a, GStorable b) => GStorable (Flat    a b) 
instance (GStorable a, GStorable b) => GStorable (Nested  a b)
instance (GStorable a, GStorable b) => GStorable (Nested2 a b)
