
module TagIntegrationTests ( tests ) where

import qualified Data.HashSet     as Set

import           Database.EJDB2

import           Media

import           Read

import           Repository       ( mediaCollection )

import           Tag

import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = withResource initDatabase
                     close
                     (\databaseIO ->
                      testGroup "TagIntegration" [ addTags databaseIO ])

initDatabase :: IO Database
initDatabase = open (minimalOptions "./test/tags.ejdb" [ truncateOpenFlags ])
    >>= \database ->
    putNew database mediaCollection emptyMedia >> return database

addTags :: IO Database -> TestTree
addTags databaseIO = testCase "addTags" $ do
    database <- databaseIO
    media <- addTag database firstTag 1
    Media.tags media @?= Just (Set.singleton firstTag)
    media' <- addTag database secondTag 1
    Media.tags media' @?= Just (Set.fromList [ firstTag, secondTag ])
    media'' <- addTag database firstTag 1
    Media.tags media'' @?= Just (Set.fromList [ firstTag, secondTag ])
    media''' <- getMediaById database 1
    Media.tags media''' @?= Just (Set.fromList [ firstTag, secondTag ])
    tags <- getTags database
    tags @?= expectedTags
  where
    firstTag = "first tag"

    secondTag = "second"

    expectedTags = [ Tag firstTag, Tag secondTag ]

