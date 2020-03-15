{-# LANGUAGE OverloadedStrings #-}

module ReadMediaTests ( tests ) where

import           Control.Exception

import           Data.Text

import           Media

import           Prelude           hiding ( catch )

import           Repository

import           System.Directory
import           System.IO.Error   hiding ( catch )

import           Test.Tasty
import           Test.Tasty.HUnit

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where
    handleExists e
        | isDoesNotExistError e = return ()
        | otherwise = throwIO e

tests :: TestTree
tests = testGroup "Repository" [ createEmptyDatabase, readSomeMedia ]

createEmptyDatabase :: TestTree
createEmptyDatabase =
    withResource (Repository.create "./test/new-empty.sqlite3")
                 (\repository -> do
                      Repository.release repository
                      removeIfExists "./test/new-empty.sqlite3")
                 (\m -> testCase "createEmptyDatabase"
                                 (do
                                      repository <- m
                                      photos <- Repository.getMedia repository
                                                                    Repository.defaultMediaQuery
                                      [] @=? photos))

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
