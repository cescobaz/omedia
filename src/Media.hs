{-# LANGUAGE DeriveGeneric #-}

module Media ( Media(..), isContentTypeAllowed, isSuffixAllowed ) where

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

allowedContentType :: [String]
allowedContentType = [ "image" ]

isContentTypeAllowed :: String -> Bool
isContentTypeAllowed t = elem t allowedContentType

isSuffixAllowed :: String -> Bool
isSuffixAllowed ".jpeg" = True
isSuffixAllowed ".jpg" = True
isSuffixAllowed ".png" = True
isSuffixAllowed _ = False
