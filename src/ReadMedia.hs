{-# LANGUAGE OverloadedStrings #-}

module ReadMedia
    ( getApiMedia
    , MediaQuery(..)
    , getMedia
    , defaultMediaQuery
    , fromFile
    , metadataFromFile
    , utcDateFromMetadata
    ) where

import           Control.Monad.IO.Class

import qualified Data.Map.Strict        as Map
import           Data.Maybe
import           Data.Text
import qualified Data.Time.Clock        as Clock
import qualified Data.Time.Format       as TimeFormat
import           Data.Word

import           Database.EJDB2
import           Database.EJDB2.Query   as Query

import qualified Graphics.HsExif        as E

import           Media

import           Repository

import           Text.Regex

import           Web.Scotty

data MediaQuery = MediaQuery { offset :: Int, limit :: Int }

getApiMedia :: Repository -> ScottyM ()
getApiMedia repository = get "/api/media/" $ do
    let query = defaultMediaQuery
    media <- liftIO $ getMedia repository query
    json media

defaultMediaQuery :: MediaQuery
defaultMediaQuery = MediaQuery { offset = 0, limit = 25 }

getMedia :: Repository -> MediaQuery -> IO [Media]
getMedia (Repository _ database) mediaQuery = do
    query <- Query.fromString "@media/* | desc /date desc /importDate skip :offset limit :limit"
    Query.setI64 (fromIntegral $ offset mediaQuery) "offset" query
    Query.setI64 (fromIntegral $ limit mediaQuery) "limit" query
    catMaybes <$> getList' database query

fromFile :: String -> IO Media
fromFile filePath = do
    metadata <- metadataFromFile filePath
    return $ (minimalMedia 0 filePath) { metadata = Just metadata
                                       , date     = utcDateFromMetadata metadata
                                       }

metadataFromFile :: String -> IO Metadata
metadataFromFile filePath = do
    result <- E.parseFileExif filePath
    case result of
        Left message -> fail message
        Right metadatas -> do
            let metadatas' = Map.mapKeys (\(E.ExifTag _ _ k _) -> k) metadatas
            return $ mapMetadata metadatas'

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

utcDateFromMetadata :: Metadata -> Maybe String
utcDateFromMetadata metadata = normalizeDateString =<< case date of
    Just date -> Just date
    Nothing -> maybe date'' Just date'
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
