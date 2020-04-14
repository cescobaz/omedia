module Main where

import           CreateMediaFromFile

import           CreateThumbnailsTests

import           ReadTests

import           TagTests

import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain Main.tests

tests :: TestTree
tests = testGroup "Tests"
                  [ ReadTests.tests
                  , CreateMediaFromFile.tests
                  , CreateThumbnailsTests.tests
                  , TagTests.tests
                  ]
