{-# LANGUAGE DeriveGeneric #-}

module Media
    ( Media(..)
    , Metadata(..)
    , emptyMetadata
    , minimalMedia
    , isContentTypeAllowed
    , isSuffixAllowed
    , fromFile
    ) where

import           Data.Aeson       ( FromJSON, ToJSON )
import qualified Data.Map.Strict  as Map
import           Data.Maybe
import qualified Data.Time.Clock  as Clock
import qualified Data.Time.Format as TimeFormat
import           Data.Word

import           GHC.Generics

import qualified Graphics.HsExif  as E

import           Text.Regex

data Metadata =
    Metadata { dateTimeOriginal :: Maybe String
             , subSecTimeOriginal :: Maybe String
             , offsetTimeOriginal :: Maybe String
             , dateTime :: Maybe String
             , offsetTime :: Maybe String
             , subSecTime :: Maybe String
             , dateTimeDigitized :: Maybe String
             , subSecTimeDigitized :: Maybe String
             , offsetTimeDigitized :: Maybe String
             , orientation :: Maybe Int
             }
    deriving ( Eq, Show, Generic )

instance ToJSON Metadata

instance FromJSON Metadata

emptyMetadata :: Metadata
emptyMetadata =
    Metadata { dateTimeOriginal = Nothing
             , subSecTimeOriginal = Nothing
             , offsetTimeOriginal = Nothing
             , dateTime = Nothing
             , offsetTime = Nothing
             , subSecTime = Nothing
             , dateTimeDigitized = Nothing
             , subSecTimeDigitized = Nothing
             , offsetTimeDigitized = Nothing
             , orientation = Nothing
             }

data Media = Media { id         :: Int
                   , filePath   :: String
                   , importDate :: Maybe String
                   , date       :: Maybe String
                   , tags       :: Maybe [String]
                   , metadata   :: Maybe Metadata
                   }
    deriving ( Eq, Show, Generic )

instance ToJSON Media

instance FromJSON Media

minimalMedia :: Int -> String -> Media
minimalMedia id filePath =
    Media { Media.id   = id
          , filePath   = filePath
          , importDate = Nothing
          , date       = Nothing
          , tags       = Nothing
          , metadata   = Nothing
          }

allowedContentType :: [String]
allowedContentType = [ "image" ]

isContentTypeAllowed :: String -> Bool
isContentTypeAllowed t = t `elem` allowedContentType

isSuffixAllowed :: String -> Bool
isSuffixAllowed ".jpeg" = True
isSuffixAllowed ".jpg" = True
isSuffixAllowed ".png" = True
isSuffixAllowed _ = False

fromFile :: String -> IO Media
fromFile filePath = do
    result <- E.parseFileExif filePath
    case result of
        Left message -> fail message
        Right metadatas -> do
            let metadatas' = Map.mapKeys (\(E.ExifTag _ _ k _) -> k) metadatas
            let metadata = mapMetadata metadatas'
            return $
                (minimalMedia 0 filePath) { metadata = Just metadata
                                          , date     = dateFromMetadata metadata
                                          }

mapMetadata :: Map.Map Word16 E.ExifValue -> Metadata
mapMetadata metadatas =
    Metadata { dateTimeOriginal = parseExifTag 0x9003 parseExifString metadatas
             , subSecTimeOriginal =
                   parseExifTag 0x9291 parseExifString metadatas
             , offsetTimeOriginal =
                   parseExifTag 0x9011 parseExifString metadatas
             , dateTime = parseExifTag 0x0132 parseExifString metadatas
             , subSecTime = parseExifTag 0x9290 parseExifString metadatas
             , offsetTime = parseExifTag 0x9010 parseExifString metadatas
             , dateTimeDigitized =
                   parseExifTag 0x9004 parseExifString metadatas
             , subSecTimeDigitized =
                   parseExifTag 0x9292 parseExifString metadatas
             , offsetTimeDigitized =
                   parseExifTag 0x9012 parseExifString metadatas
             , orientation = parseExifTag 0x0112 parseExifInt metadatas
             }

parseExifTag :: Word16
             -> (E.ExifValue -> Maybe a)
             -> Map.Map Word16 E.ExifValue
             -> Maybe a
parseExifTag code parse metadatas = Map.lookup code metadatas >>= parse

parseExifInt :: E.ExifValue -> Maybe Int
parseExifInt (E.ExifNumber number) = Just $ fromIntegral number
parseExifInt _ = Nothing

parseExifString :: E.ExifValue -> Maybe String
parseExifString (E.ExifText string) = Just string
parseExifString _ = Nothing

dateFromMetadata :: Metadata -> Maybe String
dateFromMetadata metadata = maybe Nothing normalizeDateString $ case date of
    Just date -> Just date
    Nothing -> case date' of
        Just date' -> Just date'
        Nothing -> date''
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
    return $
        TimeFormat.formatTime TimeFormat.defaultTimeLocale "%Y-%m-%dT%T%QZ" utc
