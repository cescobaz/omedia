module CreateThumbnailsTests where

import           Codec.Picture

import           CreateMediaThumbnails

import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests =
    testGroup "CreateThumbnails" [ createThumbnailTest, createThumbnailsTest ]

createThumbnailTest :: TestTree
createThumbnailTest = testCase "createThumbnailTest" $ do
    thumbnail <- createThumbnail "./test/big-image.jpeg"
                                 "./test/data-out/thumbnails"
                                 size
    Right image <- readImage ("./test/data-out/thumbnails/" ++ thumbnail)
    dynamicMap imageWidth image @?= fst size
    dynamicMap imageHeight image @?= 96
  where
    size = (128, 128)

createThumbnailsTest :: TestTree
createThumbnailsTest = testCase "createThumbnailsTest" $ do
    thumbnails <- createThumbnails "./test/big-image.jpeg"
                                   "./test/data-out/thumbnails"
                                   sizes
    mapM_ (\(thumbnail, size) -> do
               Right image
                   <- readImage ("./test/data-out/thumbnails/" ++ thumbnail)
               let expectedWidth = fst size
               let expectedHeight =
                       round (0.75 * fromIntegral (snd size) :: Double)
               dynamicMap imageWidth image @?= expectedWidth
               dynamicMap imageHeight image @?= expectedHeight)
          (zip thumbnails sizes)
  where
    sizes = [ (512, 512), (256, 256), (128, 128) ]
