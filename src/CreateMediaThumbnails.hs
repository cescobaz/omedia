{-# LANGUAGE FlexibleContexts #-}

module CreateMediaThumbnails where

import           Codec.Picture
import           Codec.Picture.Extra
import           Codec.Picture.Types

createThumbnail :: String -> String -> (Int, Int) -> IO String
createThumbnail filePath destFolder maxSize = readImage filePath
    >>= either fail (return . ImageRGB16 . scaleImage maxSize . convertRGB16)
    >>= saveJpgImage 100 destFilePath >> return thumbnailFileName
  where
    fileName = "pippo.jpeg"

    thumbnailFileName = fileName ++ show (fst maxSize) ++ ".jpeg"

    destFilePath = destFolder ++ "/" ++ thumbnailFileName

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
