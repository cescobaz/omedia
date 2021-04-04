module Main where

import           CreateMediaFromFile

import           CreateThumbnailsTests

import           ReadExif

import           ReadTests

import           TagIntegrationTests

import           TagTests

import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain Main.tests

tests :: TestTree
tests = testGroup "Tests"
                  [ ReadExif.tests
                  , ReadTests.tests
                  , CreateMediaFromFile.tests
                  , CreateThumbnailsTests.tests
                  , TagTests.tests
                  , TagIntegrationTests.tests
                  ]
