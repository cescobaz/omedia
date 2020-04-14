{-# LANGUAGE OverloadedStrings #-}

module TagTests ( tests ) where

import qualified Data.HashSet     as Set

import           Media

import           Tag

import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Tag"
                  [ addFirstTagTest
                  , addTagTest
                  , removeNotExistingTagTest
                  , removeTagTest
                  ]

addFirstTagTest :: TestTree
addFirstTagTest = testCase "addFirstTag" $ do
    addTagToMedia tag media @?= expectedMedia
    addTagToMedia tag media' @?= expectedMedia
  where
    media = emptyMedia { tags = Nothing }

    media' = emptyMedia { tags = Just Set.empty }

    expectedMedia = emptyMedia { tags = Just (Set.singleton tag) }

    tag = "first"

addTagTest :: TestTree
addTagTest = testCase "addTag" $ addTagToMedia tag media @?= expectedMedia
  where
    media =
        emptyMedia { tags = Just $ Set.fromList [ "first", "second", "third" ]
                   }

    expectedMedia =
        emptyMedia { tags = Just $
                         Set.fromList [ "first", "second", "third", tag ]
                   }

    tag = "last"

removeNotExistingTagTest :: TestTree
removeNotExistingTagTest = testCase "removeNotExistingTag" $ do
    removeTagFromMedia tag media @?= media
    removeTagFromMedia tag media' @?= media'
    removeTagFromMedia tag media'' @?= media''
  where
    media = emptyMedia { tags = Nothing }

    media' = emptyMedia { tags = Just Set.empty }

    media'' = emptyMedia { tags = Just $ Set.fromList [ "first", "second" ] }

    tag = "not existing tag"

removeTagTest :: TestTree
removeTagTest = testCase "removeTag" $ do
    removeTagFromMedia "first" media @?= expectedMedia
    removeTagFromMedia "second" media' @?= expectedMedia'
  where
    media = emptyMedia { tags = Just $ Set.fromList [ "first", "second" ] }

    expectedMedia = emptyMedia { tags = Just $ Set.fromList [ "second" ] }

    media' = expectedMedia

    expectedMedia' = emptyMedia { tags = Just Set.empty }

