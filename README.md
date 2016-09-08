# Introduction

The goal of `generic-storable-plugin` is to support the [generic-storable](https://www.github.com/mkloczko/generic-storable) package. It introduces optimisations to GStorable methods derived using GHC.Generics at core-to-core passes. 

# Usage

Just add a `-fplugin=Foreign.Storable.Generic.Plugin` flag and you're set. You might also want to pass a verbosity flag -vX, where X is either 0,1 or 2. By default the verbosity flag is set to `-v1`. 


```haskell
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt=Foreign.Storable.Generic.Plugin:-v1 #-} 
module Main where

import GHC.Generics

import Foreign.Storable.Generic
import Foreign.Ptr
import Foreign.Marshal.Alloc

data Point = Point {
    x :: Float,
    y :: Float
    } deriving (Show, Read, Generic, GStorable)

main = do
    let val = Point 0.0 10.0
    ptr <- malloc :: IO (Ptr Point)
    putStrLn "Created a ptr with value of"
    print =<< peek ptr
    poke ptr val
    putStrLn "And now the value of ptr is:"
    print =<< peek ptr
```

# Benchmarks

