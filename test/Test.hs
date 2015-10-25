{-# LANGUAGE OverloadedStrings #-}

-------------------------------------------------------------------------------

module Main where

-------------------------------------------------------------------------------

import           CircuitBreaker

import qualified Test.Tasty                as Tasty
import qualified Test.Tasty.SmallCheck     as SmallCheck
import qualified Test.Tasty.QuickCheck     as QuickCheck
import qualified Test.Tasty.HUnit          as HUnit

-------------------------------------------------------------------------------

main :: IO ()
main = Tasty.defaultMain testSuite

-------------------------------------------------------------------------------

testSuite :: Tasty.TestTree
testSuite = Tasty.testGroup "Test Suite"
  [ props
  , units
  ]

-------------------------------------------------------------------------------

props :: Tasty.TestTree
props = Tasty.testGroup "Properties"
  [ QuickCheck.testProperty "forall n. n == id n" $
      \n -> (n :: Int) == id n
  , SmallCheck.testProperty "forall n. n < n + 1" $
      \n -> (n :: Int) < succ n
  ]

-------------------------------------------------------------------------------

units :: Tasty.TestTree
units = Tasty.testGroup "Unit Tests"
  [ HUnit.testCase "Arithmetic" $ do
      let expected = 42 :: Int
      let actual = 6 * 7
      HUnit.assertEqual "multiplication should return expected value"
                         expected actual
  ]
