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
    thumbnails <- createThumbnails "./test/big-image.jpeg"
                                   "./test/data-out/thumbnails"
                                   sizes
    mapM_ (uncurry testThumbnail) (zip sizes thumbnails)
  where
    sizes = [ (512, 512), (256, 256), (128, 128) ]
