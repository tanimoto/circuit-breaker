{-# LANGUAGE DeriveDataTypeable #-}

-------------------------------------------------------------------------------

module Main where

-------------------------------------------------------------------------------

import           CircuitBreaker.Internal

import           Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM as STM
import           Control.Concurrent.STM.TVar (TVar)
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Control.Exception as Exception
import           Control.Monad (void, when)
import qualified Data.Time as Time
import           Data.Typeable (Typeable)
import           System.Timeout (timeout)
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit

-------------------------------------------------------------------------------

main :: IO ()
main = Tasty.defaultMain testSuite

-------------------------------------------------------------------------------

testSuite :: Tasty.TestTree
testSuite = Tasty.testGroup "CircuitBreaker"
  [ newCircuitBreakerTests
  , circuitBreakerStatusTests
  , resetCircuitBreakerTests
  , tripCircuitBreakerTests
  , withCircuitBreakerTests
  ]

-------------------------------------------------------------------------------

newCircuitBreakerTests :: Tasty.TestTree
newCircuitBreakerTests = Tasty.testGroup "CircuitBreaker.newCircuitBreaker"
  [ HUnit.testCase "newCircuitBreaker should accept valid values" $ do
      let validValues =
            [ (1, 1)
            , (1, 0)
            , (10, 1)
            , (1, 10)
            ]

      let accept (maxFailures, resetTimeout) = do
            circuit <- newCircuitBreaker maxFailures resetTimeout
            verifyCircuitBreakerWorks circuit

      mapM_ accept validValues

  , HUnit.testCase "newCircuitBreaker should reject invalid values" $ do
      let invalidValues =
            [ (0, 1)
            , ((-1), 1)
            , (1, (-1))
            ]

      let reject (maxFailures, resetTimeout) =
            expectSomeException (newCircuitBreaker maxFailures resetTimeout)

      mapM_ reject invalidValues
  ]


circuitBreakerStatusTests :: Tasty.TestTree
circuitBreakerStatusTests = Tasty.testGroup "CircuitBreaker.circuitBreakerStatus"
  [ HUnit.testCase "circuitBreakerStatus should report open status" $ do
      circuit <- newCircuitBreaker 1 1
      tripCircuitBreaker circuit
      verifyCircuitBreakerStatus circuit CircuitBreakerOpen

  , HUnit.testCase "circuitBreakerStatus should report half-open status" $ do
      circuit <- newCircuitBreaker 1 0.1
      tripCircuitBreaker circuit
      withDelay shortDelay
      verifyCircuitBreakerStatus circuit CircuitBreakerHalfOpen

  , HUnit.testCase "circuitBreakerStatus should report closed status" $ do
      circuit <- newCircuitBreaker 1 1
      verifyCircuitBreakerStatus circuit CircuitBreakerClosed

  ]


resetCircuitBreakerTests :: Tasty.TestTree
resetCircuitBreakerTests = Tasty.testGroup "CircuitBreaker.resetCircuitBreaker"
  [ HUnit.testCase "resetCircuitBreaker should reset failures to zero" $ do
      circuit <- newCircuitBreaker 1 1
      tripCircuitBreaker circuit
      resetCircuitBreaker circuit

      state <- STM.atomically $ TVar.readTVar $ circuitBreakerState circuit
      HUnit.assertEqual "Failures did not match" 0 (circuitBreakerStateFailures state)

  , HUnit.testCase "resetCircuitBreaker should enable circuit to work again" $ do
      circuit <- newCircuitBreaker 1 1
      tripCircuitBreaker circuit
      verifyCircuitBreakerStatus circuit CircuitBreakerOpen

      resetCircuitBreaker circuit
      verifyCircuitBreakerWorks circuit
      verifyCircuitBreakerStatus circuit CircuitBreakerClosed
  ]

tripCircuitBreakerTests :: Tasty.TestTree
tripCircuitBreakerTests = Tasty.testGroup "CircuitBreaker.tripCircuitBreaker"
  [ HUnit.testCase "tripCircuitBreaker should increase failures and update last failure" $ do
      startTime <- Time.getCurrentTime
      circuit <- newCircuitBreaker 1 1
      withDelay shortDelay
      tripCircuitBreaker circuit

      state <- STM.atomically $ TVar.readTVar $ circuitBreakerState circuit
      HUnit.assertEqual "Failures did not match" 1 (circuitBreakerStateFailures state)
      HUnit.assertBool "LastFailure was not updated" (startTime < circuitBreakerStateLastFailure state)

  , HUnit.testCase "tripCircuitBreaker should break circuit when failures above threshold" $ do
      circuit <- newCircuitBreaker 1 1
      tripCircuitBreaker circuit
      verifyCircuitBreakerStatus circuit CircuitBreakerOpen
      verifyCircuitBreakerFails circuit

  , HUnit.testCase "tripCircuitBreaker should not break circuit when failures below threshold" $ do
      circuit <- newCircuitBreaker 2 1
      tripCircuitBreaker circuit
      verifyCircuitBreakerStatus circuit CircuitBreakerClosed
      verifyCircuitBreakerWorks circuit
  ]

withCircuitBreakerTests :: Tasty.TestTree
withCircuitBreakerTests = Tasty.testGroup "CircuitBreaker.withCircuitBreaker"
  [ HUnit.testCase "withCircuitBreaker should allow action to run when in closed status" $ do
      circuit <- newCircuitBreaker 1 1
      counter <- newCounter
      verifyCircuitBreakerStatus circuit CircuitBreakerClosed
      withCircuitBreaker circuit (incrementCounter counter)
      actionCount <- readCounter counter
      HUnit.assertEqual "Action did not run" 1 actionCount

  , HUnit.testCase "withCircuitBreaker should not allow action to run when in open status" $ do
      circuit <- newCircuitBreaker 1 1
      tripCircuitBreaker circuit
      verifyCircuitBreakerStatus circuit CircuitBreakerOpen
      counter <- newCounter
      expectSomeException $ withCircuitBreaker circuit (incrementCounter counter)
      actionCount <- readCounter counter
      HUnit.assertEqual "Action did run" 0 actionCount

  , HUnit.testCase "withCircuitBreaker should allow one action to run when in half-open status" $ do
      circuit <- newCircuitBreaker 1 0
      tripCircuitBreaker circuit
      verifyCircuitBreakerStatus circuit CircuitBreakerHalfOpen
      counter <- newCounter
      withCircuitBreaker circuit (incrementCounter counter)
      actionCount <- readCounter counter
      HUnit.assertEqual "Action did not run" 1 actionCount

  , HUnit.testCase "withCircuitBreaker should reset circuit when half-open action succeeds" $ do
      circuit <- newCircuitBreaker 1 0
      tripCircuitBreaker circuit
      verifyCircuitBreakerStatus circuit CircuitBreakerHalfOpen
      counter <- newCounter
      withCircuitBreaker circuit (incrementCounter counter)
      actionCount <- readCounter counter
      HUnit.assertEqual "Action did not run" 1 actionCount
      verifyCircuitBreakerStatus circuit CircuitBreakerClosed
      verifyCircuitBreakerWorks circuit

  , HUnit.testCase "withCircuitBreaker should trip circuit when half-open action fails" $ do
      circuit <- newCircuitBreaker 1 0.1
      tripCircuitBreaker circuit

      withDelay shortDelay

      verifyCircuitBreakerStatus circuit CircuitBreakerHalfOpen
      counter <- newCounter
      expectSomeException $ withCircuitBreaker circuit (incrementCounter counter >> throwTestException)
      actionCount <- readCounter counter
      HUnit.assertEqual "Action did not run" 1 actionCount

      verifyCircuitBreakerStatus circuit CircuitBreakerOpen
      verifyCircuitBreakerFails circuit

  , HUnit.testCase "withCircuitBreaker should work after synchronous exception" $ do
      circuit <- newCircuitBreaker 2 1

      expectSomeException $ withCircuitBreaker circuit throwTestException

      verifyCircuitBreakerWorks circuit

  , HUnit.testCase "withCircuitBreaker should work after asynchronous exception" $ do
      circuit <- newCircuitBreaker 2 1

      barrier <- newBarrier
      void $ withTimeout shortDelay $ withCircuitBreaker circuit (waitBarrier barrier)

      verifyCircuitBreakerWorks circuit

  , HUnit.testCase "withCircuitBreaker should allow multiple threads to run when in closed status" $ do
      let threads = 100
      let expected = threads

      circuit <- newCircuitBreaker 1 1

      verifyCircuitBreakerStatus circuit CircuitBreakerClosed
      counter <- newCounter
      void $ flip Async.mapConcurrently [1..threads] $ \_ ->
          withCircuitBreaker circuit (incrementCounter counter)

      actual <- readCounter counter
      HUnit.assertEqual "Multiple threads did not run" expected actual

  , HUnit.testCase "withCircuitBreaker should not allow multiple threads to run when in open status" $ do
      let threads = 100 :: Int
      let expected = 0

      circuit <- newCircuitBreaker 1 1
      tripCircuitBreaker circuit
      verifyCircuitBreakerStatus circuit CircuitBreakerOpen

      counter <- newCounter
      flip mapM_ [1..threads] $ \_ ->
          Async.async $ withCircuitBreaker circuit (incrementCounter counter)

      actual <- readCounter counter
      HUnit.assertEqual "Multiple threads did not run" expected actual

  , HUnit.testCase "withCircuitBreaker should only allow a single thread to run when in half-open status" $ do
      let threads = 100 :: Int
      let expected = 1

      circuit <- newCircuitBreaker 1 0.1
      tripCircuitBreaker circuit
      withDelay shortDelay
      verifyCircuitBreakerStatus circuit CircuitBreakerHalfOpen

      barrier <- newBarrier
      counter <- newCounter
      flip mapM_ [1..threads] $ \_ ->
          Async.async $ withCircuitBreaker circuit
            (incrementCounter counter >> waitBarrier barrier)

      actual <- readCounter counter
      HUnit.assertEqual "Did not run a single thread" expected actual

  ]

-------------------------------------------------------------------------------

data TestException = TestException
  deriving (Eq, Show, Typeable)

instance Exception.Exception TestException

throwTestException :: IO ()
throwTestException = Exception.throwIO TestException

-------------------------------------------------------------------------------

withDelay :: Int -> IO ()
withDelay millis = threadDelay (millis * 1000)

withTimeout :: Int -> IO a -> IO (Maybe a)
withTimeout millis = timeout (millis * 1000)

shortDelay :: Int
shortDelay = 100

verifyCircuitBreakerWorks :: CircuitBreaker -> IO ()
verifyCircuitBreakerWorks circuit =
  withCircuitBreaker circuit (return ())

verifyCircuitBreakerFails :: CircuitBreaker -> IO ()
verifyCircuitBreakerFails circuit =
  expectSomeException $ verifyCircuitBreakerWorks circuit

verifyCircuitBreakerStatus :: CircuitBreaker -> CircuitBreakerStatus -> IO ()
verifyCircuitBreakerStatus circuit expected = do
  actual <- circuitBreakerStatus circuit
  HUnit.assertEqual "Circuit breaker status did not match" expected actual

expectSomeException :: IO a -> IO ()
expectSomeException action = do
  res <- Exception.try action
  case res of
    Left (Exception.SomeException _) -> return ()
    _ -> HUnit.assertFailure "Did not throw any exception"

-------------------------------------------------------------------------------

newBarrier :: IO (MVar ())
newBarrier = MVar.newEmptyMVar

waitBarrier :: MVar () -> IO ()
waitBarrier = void . MVar.readMVar

openBarrier :: MVar () -> IO ()
openBarrier = flip MVar.putMVar ()

-------------------------------------------------------------------------------

newCounter :: IO (TVar Int)
newCounter = TVar.newTVarIO 0

readCounter :: TVar Int -> IO Int
readCounter = STM.atomically . TVar.readTVar

incrementCounter :: TVar Int -> IO ()
incrementCounter counter = STM.atomically $ TVar.modifyTVar' counter succ

waitCounter :: TVar Int -> Int -> IO ()
waitCounter counter expected = STM.atomically $ do
  actual <- TVar.readTVar counter
  when (expected /= actual) STM.retry
