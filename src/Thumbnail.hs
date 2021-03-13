{-# LANGUAGE DeriveGeneric #-}

module Thumbnail where

import           Data.Aeson     ( FromJSON, ToJSON )

import           Database.EJDB2 ( FromJBL, ToJBL )

import           GHC.Generics

data Thumbnail = Thumbnail { filePath :: String, width :: Int, height :: Int }
    deriving ( Show, Eq, Generic )

instance ToJSON Thumbnail

instance FromJSON Thumbnail

instance ToJBL Thumbnail

instance FromJBL Thumbnail
