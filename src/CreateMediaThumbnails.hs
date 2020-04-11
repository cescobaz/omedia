{-# LANGUAGE OverloadedStrings #-}

module CreateMediaThumbnails
    ( postApiMediaThumbnails
    , updateMediaThumbnails
    , createThumbnails
    , createThumbnail
    ) where

import           Codec.Picture
import           Codec.Picture.Extra
import           Codec.Picture.Types

import           Control.Monad.IO.Class

import           Data.Maybe
import qualified Data.Text              as T

import qualified Database.EJDB2         as DB

import           File

import           Folders

import qualified Media

import           Repository

import           System.FilePath

import           Thumbnail

import           UpdateMedia

import           Web.Scotty

postApiMediaThumbnails :: Repository -> ScottyM ()
postApiMediaThumbnails (Repository homePath database) =
    post "/api/media/:id/thumbnails"
         (fromIntegral . read <$> param "id" >>= liftIO
          . updateMediaM database (updateMediaThumbnails homePath) >>= json)

updateMediaThumbnails :: T.Text -> Media.Media -> IO Media.Media
updateMediaThumbnails homePath media =
    maybe (fail "no media filePath")
          (\mediaFilePath ->
           fmap mapThumbnailsFilePath
                (createThumbnails (T.unpack homePath ++ mediaFilePath)
                                  thumbnailDirectory
                                  sizes) >>= \thumbnails ->
           maybe (return ())
                 (mapM_ (deleteFile homePath))
                 (Media.thumbnails media)
           >> return media { Media.thumbnails = Just thumbnails })
          (Media.filePath media)
  where
    thumbnailDirectory = T.unpack homePath </> mediaThumbnails

    sizes = [ (512, 512), (256, 256), (128, 128) ]

mapThumbnailsFilePath :: [Thumbnail] -> [Thumbnail]
mapThumbnailsFilePath = map mapThumbnailFilePath

mapThumbnailFilePath :: Thumbnail -> Thumbnail
mapThumbnailFilePath thumbnail =
    thumbnail { filePath = mediaThumbnails </> filePath thumbnail }

deleteFile :: T.Text -> Thumbnail -> IO ()
deleteFile homePath thumbnail =
    removeFileIfExists (T.unpack homePath </> filePath thumbnail)

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
