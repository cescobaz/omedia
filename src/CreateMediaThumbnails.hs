{-# LANGUAGE FlexibleContexts #-}

module CreateMediaThumbnails where

import           Codec.Picture
import           Codec.Picture.Extra
import           Codec.Picture.Types

import           File

import           System.FilePath.Posix

data Thumbnail = Thumbnail { filePath :: String, width :: Int, height :: Int }

createThumbnails :: String -> String -> [(Int, Int)] -> IO [Thumbnail]
createThumbnails filePath directory = mapM (createThumbnail filePath directory)

createThumbnail :: String -> String -> (Int, Int) -> IO Thumbnail
createThumbnail filePath directory maxSize = readImage filePath
    >>= either fail (return . scaleDynamicImage maxSize)
    >>= \(image, (width, height)) ->
    chooseNotExistingFileName directory (takeExtension filePath)
    >>= \destFilePath -> saveJpgImage 100 destFilePath image
    >> return Thumbnail { filePath = takeFileName destFilePath
                        , width    = width
                        , height   = height
                        }

scaleDynamicImage :: (Int, Int) -> DynamicImage -> (DynamicImage, (Int, Int))
scaleDynamicImage maxSize dynamicImage =
    (ImageRGB16 $ scaleBilinear width height image, size)
  where
    image = convertRGB16 dynamicImage

    size@(width, height) =
        scaleSize maxSize (imageWidth image, imageHeight image)

scaleSize :: (Int, Int) -> (Int, Int) -> (Int, Int)
scaleSize (maxWidth, maxHeight) (originalWidth, originalHeight)
    | (originalWidth <= maxWidth) && (originalHeight <= maxHeight) =
        (originalWidth, originalHeight)
    | widthRatio < heightRatio =
        (maxWidth, round $ widthRatio * fromIntegral originalHeight)
    | otherwise = (round $ fromIntegral originalWidth * heightRatio, maxHeight)
  where
    widthRatio = fromIntegral maxWidth / fromIntegral originalWidth :: Double

    heightRatio = fromIntegral maxHeight / fromIntegral originalHeight :: Double
