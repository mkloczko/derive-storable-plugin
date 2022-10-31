# Introduction

[![Hackage](https://img.shields.io/hackage/v/derive-storable-plugin.svg)](https://hackage.haskell.org/package/derive-storable-plugin) [![Build Status](https://github.com/mkloczko/derive-storable-plugin/workflows/Haskell-CI/badge.svg)](https://github.com/mkloczko/derive-storable-plugin/actions?query=workflow%3AHaskell-CI)

The goal of `derive-storable-plugin` is to support the [derive-storable](http://hackage.haskell.org/package/derive-storable) package. It introduces optimisations to GStorable methods derived using GHC.Generics at core-to-core passes. 

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

The plugin was benchmarked using [derive-storable-benchmark](https://www.github.com/mkloczko/derive-storable-benchmark/tree/plugin) package. The benchark measures handwritten Storable instances, raw GStorable instances and optimised-by-plugin GStorable instances. The code was compiled with `-O1` optimisation flag.
![Benchmarks](https://raw.githubusercontent.com/mkloczko/derive-storable-plugin/images/benchmarks/O1.png)
