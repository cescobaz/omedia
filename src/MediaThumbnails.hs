{-# LANGUAGE OverloadedStrings #-}

module MediaThumbnails
    ( postApiMediaThumbnails
    , updateMediaThumbnails
    , createMediaThumbnails
    , createThumbnails
    , createThumbnail
    ) where

import           Control.Monad
import           Control.Monad.IO.Class

import           Data.Maybe
import qualified Data.Text              as T

import qualified Database.EJDB2         as DB

import           File

import           Folders

import           Image

import qualified Media

import           Repository

import           System.FilePath

import           Thumbnail

import           Update

import           Web.Scotty

postApiMediaThumbnails :: Repository -> ScottyM ()
postApiMediaThumbnails (Repository homePath database) =
    post "/api/media/:id/thumbnails"
         (param "id" >>= liftIO
          . updateMediaM database (updateMediaThumbnails homePath)
          . fromIntegral . read >>= json)

updateMediaThumbnails :: T.Text -> Media.Media -> IO Media.Media
updateMediaThumbnails homePath media =
    maybe (fail "no media filePath")
          (createMediaThumbnails homePath >=> \thumbnails ->
           maybe (return ())
                 (mapM_ (deleteFile homePath))
                 (Media.thumbnails media)
           >> return media { Media.thumbnails = Just thumbnails })
          filePath
  where
    filePath = fmap (T.unpack homePath </>) (Media.filePath media)

createMediaThumbnails :: T.Text -> String -> IO [Thumbnail]
createMediaThumbnails homePath filePath =
    fmap mapThumbnailsFilePath
         (createThumbnails thumbnailDirectory sizes filePath)
  where
    thumbnailDirectory = T.unpack homePath </> mediaThumbnails

    sizes = [ (128, 128), (256, 256), (512, 512) ]

mapThumbnailsFilePath :: [Thumbnail] -> [Thumbnail]
mapThumbnailsFilePath = map mapThumbnailFilePath

mapThumbnailFilePath :: Thumbnail -> Thumbnail
mapThumbnailFilePath thumbnail =
    thumbnail { filePath = mediaThumbnails </> filePath thumbnail }

deleteFile :: T.Text -> Thumbnail -> IO ()
deleteFile homePath thumbnail =
    removeFileIfExists (T.unpack homePath </> filePath thumbnail)

createThumbnails :: String -> [(Int, Int)] -> String -> IO [Thumbnail]
createThumbnails directory sizes filePath =
    mapM (\size -> createThumbnail directory size filePath) sizes

createThumbnail :: String -> (Int, Int) -> String -> IO Thumbnail
createThumbnail directory (width, height) filePath =
    createNotExistingFileName directory ".jpg" -- convert to supported format anyway (heic is not supported by browser)
    >>= \destFilePath -> Image.scale filePath destFilePath (max width height)
    >>= \(thumbnailWidth, thumbnailHeight) ->
    return Thumbnail { filePath = takeFileName destFilePath
                     , width    = thumbnailWidth
                     , height   = thumbnailHeight
                     }

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
