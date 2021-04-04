module ReadExif ( tests ) where

import qualified Data.Map.Strict  as Map

import           Exif

import           Media

import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "ReadExif" [ readExif, parseStringTest, parseIntTest ]

readExif :: TestTree
readExif = testCase "readExif" $ do
    headers <- Exif.exif filePath
    metadata @=? headers
  where
    filePath = "./test/image-with-exif.jpeg"

    metadata =
        Map.fromList [ ( "exif-ifd2-DateTimeOriginal"
                       , Just "2019:07:15 08:55:58 (2019:07:15 08:55:58, ASCII, 20 components, 20 bytes)"
                       )
                     , ( "exif-ifd2-SubSecTimeOriginal"
                       , Just "085 (085, ASCII, 4 components, 4 bytes)"
                       )
                     , ("exif-ifd2-OffsetTimeOriginal", Nothing)
                     , ( "exif-ifd0-DateTime"
                       , Just "2019:07:15 08:55:58 (2019:07:15 08:55:58, ASCII, 20 components, 20 bytes)"
                       )
                     , ("exif-ifd2-SubSecTime", Nothing)
                     , ("exif-ifd2-OffsetTime", Nothing)
                     , ( "exif-ifd2-DateTimeDigitized"
                       , Just "2019:07:15 08:55:58 (2019:07:15 08:55:58, ASCII, 20 components, 20 bytes)"
                       )
                     , ( "exif-ifd2-SubSecTimeDigitized"
                       , Just "085 (085, ASCII, 4 components, 4 bytes)"
                       )
                     , ("exif-ifd2-OffsetTimeDigitized", Nothing)
                     , ("exif-ifd0-UniqueCameraModel", Nothing)
                     , ("exif-ifd0-LocalizedCameraModel", Nothing)
                     , ("exif-ifd0-Model", Nothing)
                     , ("exif-ifd3-GPSAltitude", Nothing)
                     , ("exif-ifd3-GPSAltitudeRef", Nothing)
                     , ("exif-ifd3-GPSLatitude", Nothing)
                     , ("exif-ifd3-GPSLatitudeRef", Nothing)
                     , ("exif-ifd3-GPSLongitude", Nothing)
                     , ("exif-ifd3-GPSLongitudeRef", Nothing)
                     , ( "exif-ifd0-Orientation"
                       , Just "1 (Top-left, Short, 1 components, 2 bytes)"
                       )
                     ]

parseStringTest :: TestTree
parseStringTest =
    testCase "parseStringTest" (expected @=? Exif.parseString value)
  where
    expected = Just "iPhone SE"

    value = "iPhone SE (iPhone SE, ASCII, 10 components, 10 bytes)"

parseIntTest :: TestTree
parseIntTest = testCase "parseIntTest" (expected @=? Exif.parseInt value)
  where
    expected = Just 1

    value = "1 (Top-left, Short, 1 components, 2 bytes)"
