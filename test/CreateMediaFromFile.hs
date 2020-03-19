module CreateMediaFromFile where

import           Media

import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "CreateMediaFromFile" [ createMediaFromImage ]

createMediaFromImage :: TestTree
createMediaFromImage = testCase "createMediaFromImage" $ do
    media <- Media.fromFile filePath
    filePath @=? Media.filePath media
    Just "2019:07:15 08:55:58" @=? Media.dateTimeOriginal media
  where
    filePath = "./test/image-with-exif.jpeg"


