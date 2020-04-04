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
    Just metadata @=? Media.metadata media
  where
    filePath = "./test/image-with-exif-2.jpeg"

    metadata = Metadata { Media.dateTimeOriginal   = Just "2020:03:07 12:05:05"
                        , Media.offsetTimeOriginal = Just "+01:00"
                        }





