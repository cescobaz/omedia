{-# LANGUAGE OverloadedStrings #-}

module MediaMetadata
    ( postApiMediaMetadata
    , updateMediaMetadata
    , metadataFromFile
    , utcDateFromMetadata
    ) where

import           Control.Applicative
import           Control.Monad.IO.Class

import qualified Data.Map.Strict        as Map
import           Data.Text
import qualified Data.Time.Clock        as Clock
import qualified Data.Time.Format       as TimeFormat
import           Data.Word

import qualified Exif

import           Media

import           Repository

import           System.FilePath

import           Text.Regex

import           Time

import           Update

import           Web.Scotty

postApiMediaMetadata :: Repository -> ScottyM ()
postApiMediaMetadata (Repository homePath database) =
    post "/api/media/:id/metadata" $ param "id" >>= liftIO
    . updateMediaM database (updateMediaMetadata homePath) . read >>= json

updateMediaMetadata :: Text -> Media -> IO Media
updateMediaMetadata homePath media =
    maybe (fail "no filePath for media")
          (\mediaFilePath ->
           Exif.exif (unpack homePath </> mediaFilePath) >>= \rawMetadata -> do
               let metadata = createMetadata rawMetadata
               return media { metadata = Just metadata
                            , date     = utcDateFromMetadata metadata
                            , gps      = Nothing
                            })
          (Media.filePath media)

metadataFromFile :: FilePath -> IO Metadata
metadataFromFile filePath = createMetadata <$> Exif.exif filePath

createMetadata :: Map.Map String Exif.Value -> Metadata
createMetadata headers =
    emptyMetadata { gpsAltitude = Exif.parseValue "exif-ifd3-GPSAltitude"
                                                  Exif.parseDouble
                                                  headers
                  , gpsAltitudeRef = Exif.parseValue "exif-ifd3-GPSAltitudeRef"
                                                     Exif.parseString
                                                     headers
                  , gpsLatitude = Exif.parseValue "exif-ifd3-GPSLatitude"
                                                  Exif.parseDoubleList
                                                  headers
                  , gpsLatitudeRef = Exif.parseValue "exif-ifd3-GPSLatitudeRef"
                                                     Exif.parseString
                                                     headers
                  , gpsLongitude = Exif.parseValue "exif-ifd3-GPSLongitude"
                                                   Exif.parseDoubleList
                                                   headers
                  , gpsLongitudeRef =
                        Exif.parseValue "exif-ifd3-GPSLongitudeRef"
                                        Exif.parseString
                                        headers
                  , dateTimeOriginal =
                        Exif.parseValue "exif-ifd2-DateTimeOriginal"
                                        Exif.parseString
                                        headers
                  , subSecTimeOriginal =
                        Exif.parseValue "exif-ifd2-SubSecTimeOriginal"
                                        Exif.parseString
                                        headers
                  , offsetTimeOriginal =
                        Exif.parseValue "exif-ifd2-OffsetTimeOriginal"
                                        Exif.parseString
                                        headers
                  , dateTime = Exif.parseValue "exif-ifd0-DateTime"
                                               Exif.parseString
                                               headers
                  , subSecTime = Exif.parseValue "exif-ifd2-SubSecTime"
                                                 Exif.parseString
                                                 headers
                  , offsetTime = Exif.parseValue "exif-ifd2-OffsetTime"
                                                 Exif.parseString
                                                 headers
                  , dateTimeDigitized =
                        Exif.parseValue "exif-ifd2-DateTimeDigitized"
                                        Exif.parseString
                                        headers
                  , subSecTimeDigitized =
                        Exif.parseValue "exif-ifd2-SubSecTimeDigitized"
                                        Exif.parseString
                                        headers
                  , offsetTimeDigitized =
                        Exif.parseValue "exif-ifd2-OffsetTimeDigitized"
                                        Exif.parseString
                                        headers
                  , orientation = Exif.parseValue "exif-ifd0-Orientation"
                                                  Exif.parseInt
                                                  headers
                  , uniqueCameraModel =
                        Exif.parseValue "exif-ifd0-UniqueCameraModel"
                                        Exif.parseString
                                        headers
                  , localizedCameraModel =
                        Exif.parseValue "exif-ifd0-LocalizedCameraModel"
                                        Exif.parseString
                                        headers
                  , model = Exif.parseValue "exif-ifd0-Model"
                                            Exif.parseString
                                            headers
                  }

utcDateFromMetadata :: Metadata -> Maybe String
utcDateFromMetadata metadata = (date <|> (date' <|> date''))
    >>= normalizeDateString
  where
    date = mkDateString (dateTimeOriginal metadata)
                        (subSecTimeOriginal metadata)
                        (offsetTimeOriginal metadata)

    date' = mkDateString (dateTimeDigitized metadata)
                         (subSecTimeDigitized metadata)
                         (offsetTimeDigitized metadata)

    date'' = mkDateString (dateTime metadata)
                          (subSecTime metadata)
                          (offsetTime metadata)

mkDateString
    :: Maybe String -> Maybe String -> Maybe String -> Maybe (String, String)
mkDateString Nothing _ _ = Nothing
mkDateString (Just dateTime) Nothing Nothing = do
    (year : month : day : time : _) <- matchRegex regex dateTime
    return (year ++ "-" ++ month ++ "-" ++ day ++ "T" ++ time, "%Y-%m-%dT%T")
  where
    regex =
        mkRegex "([0-9]{4}):([0-9]{2}):([0-9]{2}) ([0-9]{2}:[0-9]{2}:[0-9]{2})"
mkDateString dateTime (Just subSec) Nothing = do
    (dateString, _) <- mkDateString dateTime Nothing Nothing
    return (dateString ++ "." ++ subSec, "%Y-%m-%dT%T%Q")
mkDateString dateTime Nothing (Just offset) = do
    (dateString, _) <- mkDateString dateTime Nothing Nothing
    return (dateString ++ offset, "%Y-%m-%dT%T%z")
mkDateString dateTime subSec (Just offset) = do
    (dateString, _) <- mkDateString dateTime subSec Nothing
    return (dateString ++ offset, "%Y-%m-%dT%T%Q%z")

normalizeDateString :: (String, String) -> Maybe String
normalizeDateString (dateString, format) = do
    utc <- TimeFormat.parseTimeM True
                                 TimeFormat.defaultTimeLocale
                                 format
                                 dateString :: Maybe Clock.UTCTime
    return $ TimeFormat.formatTime TimeFormat.defaultTimeLocale
                                   Time.utcDateTimeFormat
                                   utc
