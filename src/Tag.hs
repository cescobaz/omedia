module Tag where

import qualified Data.HashSet as Set
import           Data.Maybe

import           Media

addTagToMedia :: String -> Media -> Media
addTagToMedia tag media = media { tags = Just tags }
  where
    tags = maybe (Set.singleton tag) (Set.insert tag) (Media.tags media)

removeTagFromMedia :: String -> Media -> Media
removeTagFromMedia tag media = media { tags = tags }
  where
    tags = Set.delete tag <$> Media.tags media
