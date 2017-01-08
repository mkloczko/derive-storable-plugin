module Main where

import Types
import Instances
import Usage 

import Foreign.Ptr

main :: IO ()
main = flatPeekByteOff nullPtr 0 >> return ()
