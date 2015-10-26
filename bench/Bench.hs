-------------------------------------------------------------------------------

module Main where

-------------------------------------------------------------------------------

import           CircuitBreaker

import           Control.Concurrent (threadDelay)
import           Criterion.Main (Benchmark, bench, bgroup)
import qualified Criterion.Main as Criterion

-------------------------------------------------------------------------------

main :: IO ()
main = do
  circuit <- newCircuitBreaker 10 60
  Criterion.defaultMain (benchmarks circuit)

-------------------------------------------------------------------------------

benchmarks :: CircuitBreaker -> [Benchmark]
benchmarks circuit =
  [ bgroup "Benchmarks"
    [ bench "without circuit breaker" $
        Criterion.nfIO delay

    , bench "with circuit breaker" $
        Criterion.nfIO (withCircuitBreaker circuit delay)
    ]
  ]
  where
  delay = threadDelay 100
