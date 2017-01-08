{-# OPTIONS_GHC -fplugin Foreign.Storable.Generic.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt=Foreign.Storable.Generic.Plugin:-crash #-} 

{-# LANGUAGE FlexibleInstances #-}
module Instances where

import Types
import Foreign.Storable.Generic

instance GStorable Flat    
instance GStorable Flat2
instance GStorable Nested  
instance GStorable Nested2 
