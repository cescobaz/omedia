module Update where

import           Data.Int

import qualified Database.EJDB2 as DB

import           Media

import           Read

import           Repository

updateMedia :: DB.Database -> (Media -> Media) -> Int64 -> IO Media
updateMedia database f id = fmap f (getMediaById database id)
    >>= \media -> DB.put database mediaCollection media id >> return media

updateMediaM :: DB.Database -> (Media -> IO Media) -> Int64 -> IO Media
updateMediaM database f id = getMediaById database id >>= f
    >>= \media -> DB.put database mediaCollection media id >> return media

