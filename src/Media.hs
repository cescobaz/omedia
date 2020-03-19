{-# LANGUAGE DeriveGeneric #-}

module Media ( Media(..), minimalMedia, isContentTypeAllowed, isSuffixAllowed ) where

import           Data.Aeson   ( ToJSON )

import           GHC.Generics

data Media = Media { id         :: Int
                   , filePath   :: String
                   , importDate :: Maybe String
                   , date       :: Maybe String
                   , tags       :: [String]
                   }
    deriving ( Eq, Show, Generic )

instance ToJSON Media

minimalMedia :: Int -> String -> Media
minimalMedia id filePath =
    Media { Media.id   = id
          , filePath   = filePath
          , importDate = Nothing
          , date       = Nothing
          , tags       = []
          }

allowedContentType :: [String]
allowedContentType = [ "image" ]

isContentTypeAllowed :: String -> Bool
isContentTypeAllowed t = elem t allowedContentType

isSuffixAllowed :: String -> Bool
isSuffixAllowed ".jpeg" = True
isSuffixAllowed ".jpg" = True
isSuffixAllowed ".png" = True
isSuffixAllowed _ = False
