{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Tag
    ( Tag(..)
    , postApiMediaTags
    , postApiMediaBulkTags
    , deleteApiMediaTagsBulk
    , addTagsBulk
    , addTags
    , addTagsToMedia
    , removeTagFromMedia
    ) where

import           Control.Monad.IO.Class

import           Data.Aeson             ( FromJSON, ToJSON )
import           Data.Char              ( toLower )
import qualified Data.HashSet           as Set
import           Data.Hashable
import           Data.Int
import qualified Data.List              as List
import           Data.Maybe

import qualified Database.EJDB2         as DB

import           GHC.Generics

import           Media

import           Repository

import           Update

import           Web.Scotty

newtype Tag = Tag { tag :: String }
    deriving ( Eq, Show, Generic )

instance ToJSON Tag

data BulkTagsRequest = BulkTagsRequest { media :: [Int64], tags :: [String] }
    deriving ( Eq, Show, Generic )

instance FromJSON BulkTagsRequest

postApiMediaTags :: Repository -> ScottyM ()
postApiMediaTags (Repository _ database) =
    post "/api/media/:id/tags/" $ jsonData >>= \tags ->
    (param "id" >>= (liftIO . addTags database tags) . read) >>= json

postApiMediaBulkTags :: Repository -> ScottyM ()
postApiMediaBulkTags (Repository _ database) = post "/api/media/tags/" $
    jsonData >>= \(BulkTagsRequest media tags) ->
    liftIO (addTagsBulk database tags media) >>= json

addTagsBulk :: DB.Database -> [String] -> [Int64] -> IO [Media]
addTagsBulk database tags = mapM (addTags database tags)

addTags :: DB.Database -> [String] -> Int64 -> IO Media
addTags database tags =
    updateMedia database (addTagsToMedia (toLowerTags tags))

addTagsToMedia :: [String] -> Media -> Media
addTagsToMedia tags media = media { Media.tags = Just tags' }
  where
    tagsSetToAdd = Set.fromList tags

    tags' = maybe tagsSetToAdd (Set.union tagsSetToAdd) (Media.tags media)

deleteApiMediaTagsBulk :: Repository -> ScottyM ()
deleteApiMediaTagsBulk (Repository _ database) = delete "/api/media/tags/" $
    jsonData >>= \(BulkTagsRequest media tags) ->
    liftIO (removeTagsBulk database tags media) >>= json

removeTagsBulk :: DB.Database -> [String] -> [Int64] -> IO [Media]
removeTagsBulk database tags = mapM (removeTags database tags)

removeTags :: DB.Database -> [String] -> Int64 -> IO Media
removeTags database tags = updateMedia database (removeTagsFromMedia tags)

removeTagsFromMedia :: [String] -> Media -> Media
removeTagsFromMedia tags media = media { Media.tags = tags' }
  where
    tags' = flip Set.difference (Set.fromList tags) <$> Media.tags media

removeTagFromMedia :: String -> Media -> Media
removeTagFromMedia tag = removeTagsFromMedia [ tag ]

toLowerTags :: [String] -> [String]
toLowerTags = map (map toLower)
