{-# LANGUAGE DeriveGeneric #-}

module Tag ( Tag(..), addTag, addTagToMedia, removeTagFromMedia ) where

import           Data.Aeson           ( FromJSON, ToJSON )
import qualified Data.HashSet         as Set
import           Data.Int
import           Data.Maybe

import qualified Database.EJDB2       as DB
import qualified Database.EJDB2.Query as Query

import           GHC.Generics

import           Media

import           Repository

import           Update

data Tag = Tag { tag :: String, count :: Int }
    deriving ( Eq, Show, Generic )

instance FromJSON Tag

instance ToJSON Tag

addTag :: DB.Database -> String -> Int64 -> IO Media
addTag database tag id = updateMedia database (addTagToMedia tag) id
    >>= \media -> putTag database tag >> return media

putTag :: DB.Database -> String -> IO Tag
putTag database tag =
    Query.fromString queryString >>= \q -> Query.setStringAtIndex tag 0 q
    >> return q >>= DB.getList database >>= incrementTagCount database tag
  where
    queryString = "@" ++ tagsCollection ++ "/[tag=:?] | desc /count limit 1"

incrementTagCount :: DB.Database -> String -> [(Int64, Maybe Tag)] -> IO Tag
incrementTagCount database _ ((id, Just t) : _) =
    DB.put database tagsCollection t' id >> return t'
  where
    t' = t { count = count t + 1 }
incrementTagCount database tag _ =
    DB.putNew database tagsCollection t >> return t
  where
    t = Tag tag 1

addTagToMedia :: String -> Media -> Media
addTagToMedia tag media = media { tags = Just tags }
  where
    tags = maybe (Set.singleton tag) (Set.insert tag) (Media.tags media)

removeTagFromMedia :: String -> Media -> Media
removeTagFromMedia tag media = media { tags = tags }
  where
    tags = Set.delete tag <$> Media.tags media
