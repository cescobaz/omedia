{-# LANGUAGE OverloadedStrings #-}

module ReadMedia ( getApiMedia, MediaQuery(..), getMedia, defaultMediaQuery ) where

import           Control.Monad.IO.Class

import           Data.Maybe
import           Data.Text

import           Database.EJDB2
import           Database.EJDB2.Query   as Query

import           Media

import           Repository

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
    catMaybes <$> (getList' database query)
