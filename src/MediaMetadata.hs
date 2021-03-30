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

import qualified Graphics.HsExif        as E

import           Media

import           Repository

import           System.FilePath

import           Text.Regex

import           Time

import           Update

import           Web.Scotty

type RawExifMetadata = Map.Map E.ExifTag E.ExifValue

postApiMediaMetadata :: Repository -> ScottyM ()
postApiMediaMetadata (Repository homePath database) =
    post "/api/media/:id/metadata" $ param "id" >>= liftIO
    . updateMediaM database (updateMediaMetadata homePath) . read >>= json

updateMediaMetadata :: Text -> Media -> IO Media
updateMediaMetadata homePath media =
    maybe (fail "no filePath for media")
          (\mediaFilePath ->
           rawMetadataFromFile (unpack homePath </> mediaFilePath)
           >>= \rawMetadata -> do
               let metadata = createMetadata rawMetadata
               return media { metadata = Just metadata
                            , date     = utcDateFromMetadata metadata
                            , gps      = gpsFromRawMetadata rawMetadata
                            })
          (Media.filePath media)

metadataFromFile :: FilePath -> IO Metadata
metadataFromFile filePath = createMetadata <$> rawMetadataFromFile filePath

createMetadata :: Map.Map E.ExifTag E.ExifValue -> Metadata
createMetadata rawMetadata =
    (mapRawMetadata rawMetadata
     . mapLowLevelMetadata (toLowLevelMetadata rawMetadata)) emptyMetadata

rawMetadataFromFile :: String -> IO (Map.Map E.ExifTag E.ExifValue)
rawMetadataFromFile filePath = do
    result <- E.parseFileExif filePath
    case result of
        Left message -> fail $ "filePath: " ++ filePath ++ " error: " ++ message
        Right metadatas -> return metadatas

gpsFromRawMetadata :: RawExifMetadata -> Maybe GPS
gpsFromRawMetadata rawMetadata = E.getGpsLatitudeLongitude rawMetadata
    >>= \(lat, lon) -> return $ GPS lat lon

toLowLevelMetadata
    :: Map.Map E.ExifTag E.ExifValue -> Map.Map Word16 E.ExifValue
toLowLevelMetadata = Map.mapKeys (\(E.ExifTag _ _ k _) -> k)

mapRawMetadata :: Map.Map E.ExifTag E.ExifValue -> Metadata -> Metadata
mapRawMetadata rawMetadata metadata =
    metadata { gpsAltitude     =
                   Map.lookup E.gpsAltitude rawMetadata >>= parseExifDouble
             , gpsAltitudeRef  =
                   Map.lookup E.gpsAltitudeRef rawMetadata >>= parseExifString
             , gpsLatitude     =
                   Map.lookup E.gpsLatitude rawMetadata >>= parseExifDoubleList
             , gpsLatitudeRef  =
                   Map.lookup E.gpsLatitudeRef rawMetadata >>= parseExifString
             , gpsLongitude    =
                   Map.lookup E.gpsLongitude rawMetadata >>= parseExifDoubleList
             , gpsLongitudeRef =
                   Map.lookup E.gpsLongitudeRef rawMetadata >>= parseExifString
             }

mapLowLevelMetadata :: Map.Map Word16 E.ExifValue -> Metadata -> Metadata
mapLowLevelMetadata lowLevelMetadata metadata =
    metadata { dateTimeOriginal =
                   parseExifTag 0x9003 parseExifString lowLevelMetadata
             , subSecTimeOriginal =
                   parseExifTag 0x9291 parseExifString lowLevelMetadata
             , offsetTimeOriginal =
                   parseExifTag 0x9011 parseExifString lowLevelMetadata
             , dateTime = parseExifTag 0x0132 parseExifString lowLevelMetadata
             , subSecTime =
                   parseExifTag 0x9290 parseExifString lowLevelMetadata
             , offsetTime =
                   parseExifTag 0x9010 parseExifString lowLevelMetadata
             , dateTimeDigitized =
                   parseExifTag 0x9004 parseExifString lowLevelMetadata
             , subSecTimeDigitized =
                   parseExifTag 0x9292 parseExifString lowLevelMetadata
             , offsetTimeDigitized =
                   parseExifTag 0x9012 parseExifString lowLevelMetadata
             , orientation = parseExifTag 0x0112 parseExifInt lowLevelMetadata
             , uniqueCameraModel =
                   parseExifTag 0xc614 parseExifString lowLevelMetadata
             , localizedCameraModel =
                   parseExifTag 0xc615 parseExifString lowLevelMetadata
             , model = parseExifTag 0x0110 parseExifString lowLevelMetadata
             }

parseExifTag :: Word16
             -> (E.ExifValue -> Maybe a)
             -> Map.Map Word16 E.ExifValue
             -> Maybe a
parseExifTag code parse metadatas = Map.lookup code metadatas >>= parse

parseExifInt :: E.ExifValue -> Maybe Int
parseExifInt (E.ExifNumber number) = Just $ fromIntegral number
parseExifInt _ = Nothing

parseExifDouble :: E.ExifValue -> Maybe Double
parseExifDouble (E.ExifRational numerator denominator) =
    Just (fromIntegral numerator / fromIntegral denominator)
parseExifDouble _ = Nothing

parseExifDoubleList :: E.ExifValue -> Maybe [Double]
parseExifDoubleList (E.ExifRationalList ratioalList) =
    Just (Prelude.map (\(n, d) -> fromIntegral n / fromIntegral d) ratioalList)
parseExifDoubleList _ = Nothing

parseExifString :: E.ExifValue -> Maybe String
parseExifString (E.ExifText string) = Just string
parseExifString _ = Nothing

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
