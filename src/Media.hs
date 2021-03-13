{-# LANGUAGE DeriveGeneric #-}

module Media
    ( Media(..)
    , Metadata(..)
    , GPS(..)
    , emptyMetadata
    , minimalMedia
    , emptyMedia
    , isContentTypeAllowed
    , isSuffixAllowed
    ) where

import           Data.Aeson     ( FromJSON, ToJSON )
import           Data.HashSet   as Set ( HashSet )
import           Data.Int

import           Database.EJDB2 ( EJDB2IDObject(..), FromJBL, ToJBL )

import           GHC.Generics

import           Prelude        hiding ( id )

import           Thumbnail      ( Thumbnail )

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
             , uniqueCameraModel :: Maybe String
             , localizedCameraModel :: Maybe String
             , model :: Maybe String
             , gpsLatitude :: Maybe [Double]
             , gpsLatitudeRef :: Maybe String
             , gpsLongitude :: Maybe [Double]
             , gpsLongitudeRef :: Maybe String
             , gpsAltitude :: Maybe Double
             , gpsAltitudeRef :: Maybe String
             }
    deriving ( Eq, Show, Generic )

instance ToJSON Metadata

instance FromJSON Metadata

instance ToJBL Metadata

instance FromJBL Metadata

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
             , uniqueCameraModel = Nothing
             , localizedCameraModel = Nothing
             , model = Nothing
             , gpsLatitude = Nothing
             , gpsLatitudeRef = Nothing
             , gpsLongitude = Nothing
             , gpsLongitudeRef = Nothing
             , gpsAltitude = Nothing
             , gpsAltitudeRef = Nothing
             }

data GPS = GPS { latitude :: Double, longitude :: Double }
    deriving ( Eq, Show, Generic )

instance ToJSON GPS

instance FromJSON GPS

instance ToJBL GPS

instance FromJBL GPS

data Media = Media { id         :: Maybe Int64
                   , filePath   :: Maybe String
                   , importDate :: Maybe String
                   , date       :: Maybe String
                   , tags       :: Maybe (Set.HashSet String)
                   , metadata   :: Maybe Metadata
                   , thumbnails :: Maybe [Thumbnail]
                   , gps        :: Maybe GPS
                   }
    deriving ( Eq, Show, Generic )

instance EJDB2IDObject Media where
    setId a m = m { id = Just a }

instance ToJSON Media

instance FromJSON Media

instance ToJBL Media

instance FromJBL Media

emptyMedia :: Media
emptyMedia = Media { Media.id   = Nothing
                   , filePath   = Nothing
                   , importDate = Nothing
                   , date       = Nothing
                   , tags       = Nothing
                   , metadata   = Nothing
                   , thumbnails = Nothing
                   , gps        = Nothing
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

