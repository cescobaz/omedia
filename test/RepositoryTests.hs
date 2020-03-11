{-# LANGUAGE OverloadedStrings #-}

module RepositoryTests ( tests ) where

import           Data.Text

import           Photo

import           Repository

import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Repository" [ readSomePhotos ]

readSomePhotos :: TestTree
readSomePhotos = withResource (Repository.create "./test/some-photos.sqlite3")
                              (Repository.release)
                              (defaultGetPhotosTest)

defaultGetPhotosTest :: IO Repository -> TestTree
defaultGetPhotosTest m =
    testCase "defaultGetPhotosTest"
             (do
                  repository <- m
                  photos <- Repository.getPhotos repository
                  expectedPhotos @=? photos)
  where
    expectedPhotos =
        [ Photo { Photo.id   = 2
                , filePath   = "/ciao-2.jpg"
                , importDate = Nothing
                , date       = Nothing
                , tags       = []
                }
        , Photo { Photo.id   = 1
                , filePath   = "/ciao.jpg"
                , importDate = Nothing
                , date       = Nothing
                , tags       = []
                }
        ]
