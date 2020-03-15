module Main where

import           ReadMediaTests

import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain Main.tests

tests :: TestTree
tests = testGroup "Tests" [ ReadMediaTests.tests ]
