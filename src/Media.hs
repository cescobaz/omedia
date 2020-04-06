{-# LANGUAGE DeriveGeneric #-}

module Media
    ( Media(..)
    , Metadata(..)
    , emptyMetadata
    , minimalMedia
    , isContentTypeAllowed
    , isSuffixAllowed
    ) where

import           Data.Aeson   ( FromJSON, ToJSON )

import           GHC.Generics

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

