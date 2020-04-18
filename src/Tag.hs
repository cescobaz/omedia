{-# LANGUAGE DeriveGeneric #-}

module Tag ( Tag(..), addTag, addTagToMedia, removeTagFromMedia ) where

import           Data.Aeson           ( FromJSON, ToJSON )
import qualified Data.HashSet         as Set
import           Data.Hashable
import           Data.Int
import           Data.Maybe

import qualified Database.EJDB2       as DB
import qualified Database.EJDB2.Query as Query

import           GHC.Generics

import           Media

import           Repository

import           Update

newtype Tag = Tag { tag :: String }
    deriving ( Eq, Show, Generic )

instance FromJSON Tag

instance ToJSON Tag

addTag :: DB.Database -> String -> Int64 -> IO Media
addTag database tag id = updateMedia database (addTagToMedia tag) id
    >>= \media -> putTag database tag >> return media

putTag :: DB.Database -> String -> IO ()
putTag database tag = print id >> DB.put database tagsCollection t id
  where
    t = Tag { tag = tag }

    id = fromIntegral (hash tag) + fromIntegral (minBound :: Int)

addTagToMedia :: String -> Media -> Media
addTagToMedia tag media = media { tags = Just tags }
  where
    tags = maybe (Set.singleton tag) (Set.insert tag) (Media.tags media)

removeTagFromMedia :: String -> Media -> Media
removeTagFromMedia tag media = media { tags = tags }
  where
    tags = Set.delete tag <$> Media.tags media
