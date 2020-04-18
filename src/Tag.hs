{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Tag
    ( Tag(..)
    , postApiMediaTags
    , addTag
    , addTagToMedia
    , removeTagFromMedia
    ) where

import           Control.Monad.IO.Class

import           Data.Aeson             ( FromJSON, ToJSON )
import qualified Data.HashSet           as Set
import           Data.Hashable
import           Data.Int
import           Data.Maybe

import qualified Database.EJDB2         as DB
import qualified Database.EJDB2.Query   as Query

import           GHC.Generics

import           Media

import           Repository

import           Update

import           Web.Scotty

newtype Tag = Tag { tag :: String }
    deriving ( Eq, Show, Generic )

instance FromJSON Tag

instance ToJSON Tag

postApiMediaTags :: Repository -> ScottyM ()
postApiMediaTags (Repository _ database) = post "/api/media/:id/tags/" $
    jsonData >>= \(Tag tag) ->
    read <$> param "id" >>= liftIO . addTag database tag >>= json

addTag :: DB.Database -> String -> Int64 -> IO Media
addTag database tag = updateMedia database (addTagToMedia tag)

addTagToMedia :: String -> Media -> Media
addTagToMedia tag media = media { tags = Just tags }
  where
    tags = maybe (Set.singleton tag) (Set.insert tag) (Media.tags media)

removeTagFromMedia :: String -> Media -> Media
removeTagFromMedia tag media = media { tags = tags }
  where
    tags = Set.delete tag <$> Media.tags media
