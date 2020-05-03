{-# LANGUAGE OverloadedStrings #-}

module ReadTests ( tests ) where

import           Control.Exception

import           Data.Text

import           Media

import           Prelude           hiding ( catch )

import           Read

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
tests = testGroup "Read" [ createEmptyDatabase, readSomeMedia ]

createEmptyDatabase :: TestTree
createEmptyDatabase =
    withResource (Repository.create "./test" "./test/new-empty.db")
                 (\repository -> do
                      Repository.release repository
                      removeIfExists "./test/new-empty.db")
                 (\m ->
                  testCase "createEmptyDatabase"
                           (do
                                repository <- m
                                photos <- getMedia repository defaultMediaQuery
                                [] @=? photos))

readSomeMedia :: TestTree
readSomeMedia =
    withResource (Repository.create "./test" "./test/some-photos.ejdb")
                 Repository.release
                 (\r ->
                  testGroup "readSomeMedia"
                            [ defaultGetMediaTest r, getMediaWithLimitTest r ])

defaultGetMediaTest :: IO Repository -> TestTree
defaultGetMediaTest m = testCase "defaultGetMediaTest"
                                 (do
                                      repository <- m
                                      photos <- getMedia repository query
                                      expectedMedia @=? photos)
  where
    query = MediaQuery { Read.tags = [], offset = 0, limit = 50 }

    expectedMedia =
        [ minimalMedia 2 "/ciao-2.jpg", minimalMedia 1 "/ciao.jpg" ]

getMediaWithLimitTest :: IO Repository -> TestTree
getMediaWithLimitTest m =
    testCase "getMediaWithLimitTest"
             (do
                  repository <- m
                  photos <- getMedia repository query
                  expectedMedia @=? photos)
  where
    query = MediaQuery { Read.tags = [], offset = 0, limit = 1 }

    expectedMedia = [ minimalMedia 2 "/ciao-2.jpg" ]
