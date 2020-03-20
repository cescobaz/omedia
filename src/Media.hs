{-# LANGUAGE DeriveGeneric #-}

module Media
    ( Media(..)
    , minimalMedia
    , isContentTypeAllowed
    , isSuffixAllowed
    , fromFile
    ) where

import qualified Codec.Picture               as P
import qualified Codec.Picture.Metadata      as M
import qualified Codec.Picture.Metadata.Exif as E

import           Data.Aeson                  ( ToJSON )
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as TE
import           Data.Word

import           GHC.Generics

data Media = Media { id :: Int
                   , filePath :: String
                   , dateTimeOriginal :: Maybe String
                   , importDate :: Maybe String
                   , date :: Maybe String
                   , tags :: [String]
                   }
    deriving ( Eq, Show, Generic )

instance ToJSON Media

minimalMedia :: Int -> String -> Media
minimalMedia id filePath =
    Media { Media.id = id
          , filePath = filePath
          , dateTimeOriginal = Nothing
          , importDate = Nothing
          , date = Nothing
          , tags = []
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
    media { dateTimeOriginal =
                parseExifTag 0x9003 parseExifStringDate metadatas
          }

parseExifTag :: Word16 -> (E.ExifData -> Maybe a) -> M.Metadatas -> Maybe a
parseExifTag code parse metadatas =
    M.lookup (M.Exif $ E.TagUnknown code) metadatas >>= parse

parseExifStringDate :: E.ExifData -> Maybe String
parseExifStringDate (E.ExifString byteString) =
    Just $ T.unpack $ T.init $ TE.decodeUtf8 byteString
parseExifStringDate _ = Nothing
