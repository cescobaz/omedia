
module TagIntegrationTests ( tests ) where

import qualified Data.HashSet         as Set
import qualified Data.HashSet         as Set

import           Database.EJDB2
import qualified Database.EJDB2.Query as Query

import           Media

import           Repository           ( mediaCollection )

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
    tags <- Query.fromString "@tags/* | asc /tag" >>= getList database
    map snd tags @?= expectedTags
  where
    firstTag = "first tag"

    secondTag = "second"

    expectedTags = [ Just $ Tag { tag = firstTag, count = 2 }
                   , Just $ Tag { tag = secondTag, count = 1 }
                   ]
