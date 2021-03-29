{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ReadTag ( getApiMediaTags, getTags ) where

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

import           Tag                    ( Tag(..) )

import           Update

import           Web.Scotty

newtype TagsContainer = TagsContainer { tags :: Maybe (Set.HashSet String) }
    deriving ( Generic )

instance DB.FromJBL TagsContainer

getApiMediaTags :: Repository -> ScottyM ()
getApiMediaTags (Repository _ database) = get "/api/media/tags/" $
    liftIO (getTags database) >>= json

getTags :: DB.Database -> IO [Tag]
getTags database = fmap Tag . List.sort . Set.toList
    <$> DB.fold database foldTags Set.empty (DB.Query query DB.noBind)
  where
    query = "@" ++ mediaCollection ++ "/* | /tags"

foldTags
    :: Set.HashSet String -> (Int64, Maybe TagsContainer) -> Set.HashSet String
foldTags set (_, Nothing) = set
foldTags set (_, Just media) = maybe set (Set.union set) (ReadTag.tags media)

