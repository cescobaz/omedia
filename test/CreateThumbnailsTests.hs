module CreateThumbnailsTests where

import           Codec.Picture

import           CreateMediaThumbnails

import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "CreateThumbnails" [ createThumbnailTest ]

createThumbnailTest :: TestTree
createThumbnailTest = testCase "createThumbnailTest" $ do
    thumbnail <- createThumbnail "./test/big-image.jpeg" "./test" size
    Right image <- readImage ("./test/" ++ thumbnail)
    dynamicMap imageWidth image @?= fst size
    dynamicMap imageHeight image @?= 96
  where
    size = (128, 128)
