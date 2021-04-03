module CreateThumbnailsTests where

import           Codec.Picture

import           MediaThumbnails

import           Test.Tasty
import           Test.Tasty.HUnit

import           Thumbnail

tests :: TestTree
tests =
    testGroup "CreateThumbnails" [ createThumbnailTest, createThumbnailsTest ]

createThumbnailTest :: TestTree
createThumbnailTest = testCase "createThumbnailTest" $ do
    thumbnail <- createThumbnail "./test/data-out/thumbnails"
                                 size
                                 "./test/big-image.jpeg"
    testThumbnail size thumbnail
  where
    size = (128, 128)

testThumbnail :: (Int, Int) -> Thumbnail -> IO ()
testThumbnail (maxWidth, maxHeight) thumbnail = do
    let expectedWidth = maxWidth
    let expectedHeight = round (0.75 * fromIntegral maxHeight :: Double)
    width thumbnail @?= expectedWidth
    height thumbnail @?= expectedHeight
    Right image
        <- readImage ("./test/data-out/thumbnails/" ++ filePath thumbnail)
    dynamicMap imageWidth image @?= expectedWidth
    dynamicMap imageHeight image @?= expectedHeight

createThumbnailsTest :: TestTree
createThumbnailsTest = testCase "createThumbnailsTest" $ do
    thumbnails <- createThumbnails "./test/data-out/thumbnails"
                                   sizes
                                   "./test/big-image.jpeg"
    mapM_ (uncurry testThumbnail) (zip sizes thumbnails)
  where
    sizes = [ (512, 512), (256, 256), (128, 128) ]
