{-# LANGUAGE DeriveDataTypeable #-}

-------------------------------------------------------------------------------

module CircuitBreaker.Internal
  ( CircuitBreaker (..)
  , CircuitBreakerStatus (..)
  , CircuitBreakerState (..)
  , CircuitBreakerException (..)
  , CircuitBreakerHalfOpenLock (..)
  , newCircuitBreaker
  , getCircuitBreakerMaxFailures
  , getCircuitBreakerResetTimeout
  , newCircuitBreakerState
  , circuitBreakerStatus
  , calculateCircuitBreakerStatus
  , resetCircuitBreaker
  , tripCircuitBreaker
  , withCircuitBreaker
  , newHalfOpenLock
  , tryWithHalfOpenLock
  ) where

-------------------------------------------------------------------------------

import           Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM as STM
import           Control.Concurrent.STM.TVar (TVar)
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Control.Exception as Exception
import           Data.Time (NominalDiffTime, UTCTime)
import qualified Data.Time as Time
import           Data.Typeable (Typeable)

-------------------------------------------------------------------------------

data CircuitBreaker = CircuitBreaker
  { circuitBreakerMaxFailures :: !Int
  , circuitBreakerResetTimeout :: !NominalDiffTime
  , circuitBreakerState :: !(TVar CircuitBreakerState)
  , circuitBreakerHalfOpenLock :: !CircuitBreakerHalfOpenLock
  }

data CircuitBreakerStatus
  = CircuitBreakerOpen
  | CircuitBreakerHalfOpen
  | CircuitBreakerClosed
  deriving (Eq, Show)

data CircuitBreakerState = CircuitBreakerState
  { circuitBreakerStateFailures :: !Int
  , circuitBreakerStateLastFailure :: !UTCTime
  }
  deriving (Eq, Show)

data CircuitBreakerException
  = CircuitBreakerOpenException
  deriving (Eq, Show, Typeable)

instance Exception.Exception CircuitBreakerException

newtype CircuitBreakerHalfOpenLock
  = CircuitBreakerHalfOpenLock (MVar ())

-------------------------------------------------------------------------------

newCircuitBreaker :: Int -> NominalDiffTime -> IO CircuitBreaker
newCircuitBreaker maxFailures resetTimeout
  | maxFailures <= 0 = error "maxFailures must be a non-negative integer"
  | resetTimeout < 0 = error "resetTimeout must be a positive number"
  | otherwise = do
      currentTime <- Time.getCurrentTime
      let failures = 0
      let state = newCircuitBreakerState failures currentTime
      stateVar <- TVar.newTVarIO state
      lock <- newHalfOpenLock
      return CircuitBreaker
        { circuitBreakerMaxFailures = maxFailures
        , circuitBreakerResetTimeout = resetTimeout
        , circuitBreakerState = stateVar
        , circuitBreakerHalfOpenLock = lock
        }

getCircuitBreakerMaxFailures :: CircuitBreaker -> Int
getCircuitBreakerMaxFailures = circuitBreakerMaxFailures

getCircuitBreakerResetTimeout :: CircuitBreaker -> NominalDiffTime
getCircuitBreakerResetTimeout = circuitBreakerResetTimeout

-------------------------------------------------------------------------------

newCircuitBreakerState :: Int -> UTCTime -> CircuitBreakerState
newCircuitBreakerState failures lastFailure =
  CircuitBreakerState
    { circuitBreakerStateFailures = failures
    , circuitBreakerStateLastFailure = lastFailure
    }

modifyCircuitBreakerState
  :: TVar CircuitBreakerState
  -> (CircuitBreakerState -> CircuitBreakerState)
  -> IO ()
modifyCircuitBreakerState state modify =
  STM.atomically $ TVar.modifyTVar' state modify

-------------------------------------------------------------------------------

circuitBreakerStatus :: CircuitBreaker -> IO CircuitBreakerStatus
circuitBreakerStatus circuit = do
  currentTime <- Time.getCurrentTime
  let maxFailures = circuitBreakerMaxFailures circuit
  let resetTimeout = circuitBreakerResetTimeout circuit
  let stateVar = circuitBreakerState circuit
  STM.atomically $ do
    state <- TVar.readTVar stateVar
    let failures = circuitBreakerStateFailures state
    let lastFailure = circuitBreakerStateLastFailure state
    return $ calculateCircuitBreakerStatus
               maxFailures failures resetTimeout lastFailure currentTime

calculateCircuitBreakerStatus
  :: Int
  -> Int
  -> NominalDiffTime
  -> UTCTime
  -> UTCTime
  -> CircuitBreakerStatus
calculateCircuitBreakerStatus maxFailures failures resetTimeout lastFailure currentTime
  | failures < maxFailures = CircuitBreakerClosed
  | Time.addUTCTime resetTimeout lastFailure <= currentTime = CircuitBreakerHalfOpen
  | otherwise = CircuitBreakerOpen

-------------------------------------------------------------------------------

resetCircuitBreaker :: CircuitBreaker -> IO ()
resetCircuitBreaker circuit = do
  let state = circuitBreakerState circuit
  modifyCircuitBreakerState state $ \s ->
    s { circuitBreakerStateFailures = 0
      }

tripCircuitBreaker :: CircuitBreaker -> IO ()
tripCircuitBreaker circuit = do
  let state = circuitBreakerState circuit
  currentTime <- Time.getCurrentTime
  modifyCircuitBreakerState state $ \s ->
    s { circuitBreakerStateFailures = circuitBreakerStateFailures s + 1
      , circuitBreakerStateLastFailure = currentTime
      }

-------------------------------------------------------------------------------

withCircuitBreaker :: CircuitBreaker -> IO a -> IO a
withCircuitBreaker circuit action = do
  let actionWithCircuitBreaker = action `Exception.onException` tripCircuitBreaker circuit
  let throwCircuitBreakerOpenException = Exception.throwIO CircuitBreakerOpenException
  status <- circuitBreakerStatus circuit
  case status of
    CircuitBreakerOpen ->
        throwCircuitBreakerOpenException
    CircuitBreakerClosed ->
        actionWithCircuitBreaker
    CircuitBreakerHalfOpen -> do
        let lock = circuitBreakerHalfOpenLock circuit
        let tryResetCircuitBreaker = do
              result <- actionWithCircuitBreaker
              resetCircuitBreaker circuit
              return result
        res <- tryWithHalfOpenLock lock tryResetCircuitBreaker
        maybe throwCircuitBreakerOpenException return res

-------------------------------------------------------------------------------

newHalfOpenLock :: IO CircuitBreakerHalfOpenLock
newHalfOpenLock = CircuitBreakerHalfOpenLock `fmap` MVar.newMVar ()

tryWithHalfOpenLock :: CircuitBreakerHalfOpenLock -> IO a -> IO (Maybe a)
tryWithHalfOpenLock (CircuitBreakerHalfOpenLock lock) action =
  Exception.bracket acquire release withLock
  where
  acquire = MVar.tryTakeMVar lock

  release Nothing = return ()
  release (Just ()) = MVar.putMVar lock ()

  withLock Nothing = return Nothing
  withLock (Just ()) = Just `fmap` action
