{-# LANGUAGE DeriveGeneric #-}

module Media
    ( Media(..)
    , Metadata(..)
    , emptyMetadata
    , minimalMedia
    , emptyMedia
    , isContentTypeAllowed
    , isSuffixAllowed
    ) where

import           Data.Aeson   ( FromJSON, ToJSON )
import           Data.Int

import           GHC.Generics

import           Thumbnail    ( Thumbnail )

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

data Media = Media { id         :: Maybe Int64
                   , filePath   :: Maybe String
                   , importDate :: Maybe String
                   , date       :: Maybe String
                   , tags       :: Maybe [String]
                   , metadata   :: Maybe Metadata
                   , thumbnails :: Maybe [Thumbnail]
                   }
    deriving ( Eq, Show, Generic )

instance ToJSON Media

instance FromJSON Media

emptyMedia :: Media
emptyMedia = Media { Media.id   = Nothing
                   , filePath   = Nothing
                   , importDate = Nothing
                   , date       = Nothing
                   , tags       = Nothing
                   , metadata   = Nothing
                   , thumbnails = Nothing
                   }

minimalMedia :: Int64 -> String -> Media
minimalMedia id filePath =
    emptyMedia { Media.id = Just id, filePath = Just filePath }

allowedContentType :: [String]
allowedContentType = [ "image" ]

isContentTypeAllowed :: String -> Bool
isContentTypeAllowed t = t `elem` allowedContentType

isSuffixAllowed :: String -> Bool
isSuffixAllowed ".jpeg" = True
isSuffixAllowed ".jpg" = True
isSuffixAllowed ".png" = True
isSuffixAllowed _ = False

