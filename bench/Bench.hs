{-# LANGUAGE OverloadedStrings #-}

-------------------------------------------------------------------------------

module Main where

-------------------------------------------------------------------------------

import           CircuitBreaker

import           Criterion.Main              (Benchmark, bench, bgroup, nf)
import qualified Criterion.Main              as Criterion

-------------------------------------------------------------------------------

main :: IO ()
main = Criterion.defaultMain benchmarks

-------------------------------------------------------------------------------

benchmarks :: [Benchmark]
benchmarks =
  [ bgroup "Benchmarks"
    [ bench "succ 1" $ nf succ (1 :: Int)
    ]
  ]
