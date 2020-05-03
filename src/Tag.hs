{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Tag
    ( Tag(..)
    , postApiMediaTags
    , getApiMediaTags
    , addTagsBulk
    , addTags
    , addTagsToMedia
    , removeTagFromMedia
    , getTags
    ) where

import           Control.Monad.IO.Class

import           Data.Aeson             ( FromJSON, ToJSON )
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

data PostTags = PostTags { media :: [Int64], tags :: [String] }
    deriving ( Eq, Show, Generic )

instance FromJSON Tag

instance ToJSON Tag

postApiMediaTags :: Repository -> ScottyM ()
postApiMediaTags (Repository _ database) = post "/api/media/:id/tags/" $
    jsonData >>= \(Tag tag) ->
    read <$> param "id" >>= liftIO . addTags database [ tag ] >>= json

addTagsBulk :: DB.Database -> [String] -> [Int64] -> IO [Media]
addTagsBulk database tags = mapM (addTags database tags)

addTags :: DB.Database -> [String] -> Int64 -> IO Media
addTags database tags = updateMedia database (addTagsToMedia tags)

addTagsToMedia :: [String] -> Media -> Media
addTagsToMedia tags media = media { Media.tags = Just tags' }
  where
    tagsSetToAdd = Set.fromList tags

    tags' = maybe tagsSetToAdd (Set.union tagsSetToAdd) (Media.tags media)

removeTagFromMedia :: String -> Media -> Media
removeTagFromMedia tag media = media { Media.tags = tags }
  where
    tags = Set.delete tag <$> Media.tags media

getApiMediaTags :: Repository -> ScottyM ()
getApiMediaTags (Repository _ database) = get "/api/media/tags/" $
    liftIO (getTags database) >>= json

getTags :: DB.Database -> IO [Tag]
getTags database = fmap Tag . List.sort . Set.toList
    <$> DB.fold database foldTags Set.empty (DB.Query query DB.noBind)
  where
    query = "@" ++ mediaCollection ++ "/*"

foldTags :: Set.HashSet String -> (Int64, Maybe Media) -> Set.HashSet String
foldTags set (_, Nothing) = set
foldTags set (_, Just media) = maybe set (Set.union set) (Media.tags media)

