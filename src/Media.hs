{-# LANGUAGE DeriveGeneric #-}

module Media ( Media(..), isSuffixAllowed ) where

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

isSuffixAllowed :: String -> Bool
isSuffixAllowed "jpeg" = True
isSuffixAllowed "jpg" = True
isSuffixAllowed "png" = True
isSuffixAllowed _ = False
