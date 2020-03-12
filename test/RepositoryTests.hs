{-# LANGUAGE OverloadedStrings #-}

module RepositoryTests ( tests ) where

import           Data.Text

import           Media

import           Repository

import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Repository" [ readSomeMedia ]

readSomeMedia :: TestTree
readSomeMedia =
    withResource (Repository.create "./test/some-photos.sqlite3")
                 Repository.release
                 (\r ->
                  testGroup "readSomeMedia"
                            [ defaultGetMediaTest r, getMediaWithLimitTest r ])

defaultGetMediaTest :: IO Repository -> TestTree
defaultGetMediaTest m =
    testCase "defaultGetMediaTest"
             (do
                  repository <- m
                  photos <- Repository.getMedia repository query
                  expectedMedia @=? photos)
  where
    query = MediaQuery { offset = 0, limit = 50 }

    expectedMedia =
        [ Media { Media.id   = 2
                , filePath   = "/ciao-2.jpg"
                , importDate = Nothing
                , date       = Nothing
                , tags       = []
                }
        , Media { Media.id   = 1
                , filePath   = "/ciao.jpg"
                , importDate = Nothing
                , date       = Nothing
                , tags       = []
                }
        ]

getMediaWithLimitTest :: IO Repository -> TestTree
getMediaWithLimitTest m =
    testCase "getMediaWithLimitTest"
             (do
                  repository <- m
                  photos <- Repository.getMedia repository query
                  expectedMedia @=? photos)
  where
    query = MediaQuery { offset = 0, limit = 1 }

    expectedMedia = [ Media { Media.id   = 2
                            , filePath   = "/ciao-2.jpg"
                            , importDate = Nothing
                            , date       = Nothing
                            , tags       = []
                            }
                    ]
