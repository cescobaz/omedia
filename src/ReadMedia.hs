{-# LANGUAGE OverloadedStrings #-}

module ReadMedia
    ( getApiMedia
    , MediaQuery(..)
    , getMedia
    , getMediaById
    , defaultMediaQuery
    ) where

import           Control.Monad.IO.Class

import           Data.Int
import qualified Data.Map.Strict        as Map
import           Data.Maybe
import           Data.Text
import qualified Data.Time.Clock        as Clock
import qualified Data.Time.Format       as TimeFormat
import           Data.Word

import           Database.EJDB2
import           Database.EJDB2.Query   as Query

import qualified Graphics.HsExif        as E

import           Media

import           Repository

import           Text.Regex

import           Web.Scotty

data MediaQuery = MediaQuery { offset :: Int, limit :: Int }

getApiMedia :: Repository -> ScottyM ()
getApiMedia repository = get "/api/media/" $ do
    let query = defaultMediaQuery
    media <- liftIO $ getMedia repository query
    json media

defaultMediaQuery :: MediaQuery
defaultMediaQuery = MediaQuery { offset = 0, limit = 25 }

getMedia :: Repository -> MediaQuery -> IO [Media]
getMedia (Repository _ database) mediaQuery = do
    query <- Query.fromString "@media/* | desc /date desc /importDate skip :offset limit :limit"
    Query.setI64 (fromIntegral $ offset mediaQuery) "offset" query
    Query.setI64 (fromIntegral $ limit mediaQuery) "limit" query
    catMaybes <$> getList' database query

getMediaById :: Database -> Int64 -> IO Media
getMediaById database id = getById database "media" id
    >>= maybe (fail "media not found")
              (\media -> return $ media { Media.id = Just id })

