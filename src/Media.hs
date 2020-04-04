{-# LANGUAGE DeriveGeneric #-}

module Media
    ( Media(..)
    , Metadata(..)
    , minimalMedia
    , isContentTypeAllowed
    , isSuffixAllowed
    , fromFile
    ) where

import qualified Codec.Picture               as P
import qualified Codec.Picture.Metadata      as M
import qualified Codec.Picture.Metadata.Exif as E

import           Data.Aeson                  ( FromJSON, ToJSON )
import           Data.Maybe
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as TE
import           Data.Word

import           GHC.Generics

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
             }
    deriving ( Eq, Show, Generic )

instance ToJSON (Metadata)

instance FromJSON (Metadata)

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
    result <- P.readImageWithMetadata filePath
    case result of
        Left message -> fail message
        Right (_, metadatas) -> do
            putStrLn "-------------------"
            putStrLn $ show $ M.extractExifMetas metadatas
            return $ addMetadatas metadatas (minimalMedia 0 filePath)

addMetadatas :: M.Metadatas -> Media -> Media
addMetadatas metadatas media =
    media { metadata =
                Just Metadata { dateTimeOriginal = parseExifTag 0x9003
                                                                parseExifString
                                                                metadatas
                              , subSecTimeOriginal =
                                    parseExifTag 0x9291
                                                 parseExifString
                                                 metadatas
                              , offsetTimeOriginal =
                                    parseExifTag 0x9011
                                                 parseExifString
                                                 metadatas
                              , dateTime =
                                    parseExifTag 0x0132 -- 0x0132 doesn't work
                                                 parseExifString
                                                 metadatas
                              , subSecTime = parseExifTag 0x9290
                                                          parseExifString
                                                          metadatas
                              , offsetTime = parseExifTag 0x9010
                                                          parseExifString
                                                          metadatas
                              , dateTimeDigitized = parseExifTag 0x9004
                                                                 parseExifString
                                                                 metadatas
                              , subSecTimeDigitized =
                                    parseExifTag 0x9292
                                                 parseExifString
                                                 metadatas
                              , offsetTimeDigitized =
                                    parseExifTag 0x9012
                                                 parseExifString
                                                 metadatas
                              }
          }

parseExifTag :: Word16 -> (E.ExifData -> Maybe a) -> M.Metadatas -> Maybe a
parseExifTag code parse metadatas =
    M.lookup (M.Exif $ E.tagOfWord16 code) metadatas >>= parse

parseExifString :: E.ExifData -> Maybe String
parseExifString (E.ExifString byteString) =
    Just $ T.unpack $ T.init $ TE.decodeUtf8 byteString
parseExifString _ = Nothing

dateFromMetadata :: Metadata -> Maybe String
dateFromMetadata metadata = case date of
    Just date -> Just date
    Nothing -> case date' of
        Just date' -> Just date'
        Nothing -> date''
  where
    date = mkDateString (dateTimeOriginal metadata)
                        (offsetTimeOriginal metadata)
                        Nothing

    date' = mkDateString (dateTimeDigitized metadata)
                         (offsetTimeDigitized metadata)
                         Nothing

    date'' = mkDateString (dateTime metadata) (offsetTime metadata) Nothing

mkDateString :: Maybe String -> Maybe String -> Maybe String -> Maybe String
mkDateString dateTime subSec offset = do
    (_ : date : time : _) <- dateTime >>= matchRegex regex
    let isoDate = date ++ "T" ++ time
    let isoDate' = maybe isoDate (++ (isoDate ++ ".")) subSec
    let isoDate'' = maybe isoDate' (++ isoDate') offset
    return isoDate''
  where
    regex = mkRegex "([0-9]{4}-[0-9]{2}-[0-9]{2}) ([0-9]{2}:[0-9]{2}:[0-9]{2})"

