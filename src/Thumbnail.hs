{-# LANGUAGE DeriveGeneric #-}

module Thumbnail where

import           Data.Aeson   ( FromJSON, ToJSON )

import           GHC.Generics

data Thumbnail = Thumbnail { filePath :: String, width :: Int, height :: Int }
    deriving ( Show, Eq, Generic )

instance ToJSON Thumbnail

instance FromJSON Thumbnail
