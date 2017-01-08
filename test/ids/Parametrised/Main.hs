module Parametrised.Main where

import Parametrised.Types
import Parametrised.Instances
import Parametrised.Usage 

import Foreign.Ptr

main :: IO ()
main = flatPeekByteOff nullPtr 0 >> return ()
