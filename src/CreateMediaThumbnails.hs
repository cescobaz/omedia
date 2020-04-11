{-# LANGUAGE FlexibleContexts #-}

module CreateMediaThumbnails where

import           Codec.Picture
import           Codec.Picture.Extra
import           Codec.Picture.Types

import           File

import           System.FilePath.Posix

createThumbnails :: String -> String -> [(Int, Int)] -> IO [String]
createThumbnails filePath directory = mapM (createThumbnail filePath directory)

createThumbnail :: String -> String -> (Int, Int) -> IO String
createThumbnail filePath directory maxSize = readImage filePath
    >>= either fail (return . ImageRGB16 . scaleImage maxSize . convertRGB16)
    >>= \image -> chooseNotExistingFileName directory (takeExtension filePath)
    >>= \destFilePath -> saveJpgImage 100 destFilePath image
    >> return (takeFileName destFilePath)

scaleImage :: ( Pixel a
              , Bounded (PixelBaseComponent a)
              , Integral (PixelBaseComponent a)
              )
           => (Int, Int)
           -> Image a
           -> Image a
scaleImage maxSize image = scaleBilinear width height image
  where
    (width, height) = scaleSize maxSize (imageWidth image, imageHeight image)

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
