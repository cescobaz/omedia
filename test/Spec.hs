module Main where

import           CreateMediaFromFile

import           CreateThumbnailsTests

import           ReadMediaTests

import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain Main.tests

tests :: TestTree
tests = testGroup "Tests"
                  [ ReadMediaTests.tests
                  , CreateMediaFromFile.tests
                  , CreateThumbnailsTests.tests
                  ]
