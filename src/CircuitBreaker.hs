-------------------------------------------------------------------------------

module CircuitBreaker
  ( CircuitBreaker
  , CircuitBreakerStatus (..)
  , CircuitBreakerException (..)
  , newCircuitBreaker
  , getCircuitBreakerMaxFailures
  , getCircuitBreakerResetTimeout
  , circuitBreakerStatus
  , resetCircuitBreaker
  , tripCircuitBreaker
  , withCircuitBreaker
  ) where

-------------------------------------------------------------------------------

import           CircuitBreaker.Internal

-------------------------------------------------------------------------------
