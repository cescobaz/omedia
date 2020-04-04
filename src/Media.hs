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
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as TE
import           Data.Word

import           GHC.Generics

data Metadata = Metadata { dateTimeOriginal   :: Maybe String
                         , offsetTimeOriginal :: Maybe String
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
        Right (_, metadatas) -> return $
            addMetadatas metadatas (minimalMedia 0 filePath)

addMetadatas :: M.Metadatas -> Media -> Media
addMetadatas metadatas media =
    media { metadata = Just Metadata { dateTimeOriginal   =
                                           parseExifTag 0x9003
                                                        parseExifStringDate
                                                        metadatas
                                     , offsetTimeOriginal =
                                           parseExifTag 0x9011
                                                        parseExifString
                                                        metadatas
                                     }
          }

parseExifTag :: Word16 -> (E.ExifData -> Maybe a) -> M.Metadatas -> Maybe a
parseExifTag code parse metadatas =
    M.lookup (M.Exif $ E.TagUnknown code) metadatas >>= parse

parseExifString :: E.ExifData -> Maybe String
parseExifString (E.ExifString byteString) =
    Just $ T.unpack $ T.init $ TE.decodeUtf8 byteString
parseExifString _ = Nothing

parseExifStringDate :: E.ExifData -> Maybe String
parseExifStringDate (E.ExifString byteString) =
    Just $ T.unpack $ T.init $ TE.decodeUtf8 byteString
parseExifStringDate _ = Nothing
