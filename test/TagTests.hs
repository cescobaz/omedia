{-# LANGUAGE OverloadedStrings #-}

module TagTests ( tests ) where

import qualified Data.HashSet     as Set

import           Media

import           Tag

import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Tag" [ addFirstTagTest ]

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
