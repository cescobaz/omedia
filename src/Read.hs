{-# LANGUAGE OverloadedStrings #-}

module Read
    ( getApiMedia
    , getApiMediaById
    , MediaQuery(..)
    , getMedia
    , getMediaById
    , defaultMediaQuery
    ) where

import           Control.Monad.IO.Class

import           Data.Int
import           Data.Maybe

import           Database.EJDB2

import           Media

import           Repository

import           Web.Scotty

data MediaQuery = MediaQuery { offset :: Int, limit :: Int }

getApiMedia :: Repository -> ScottyM ()
getApiMedia repository = get "/api/media/" $ do
    let query = defaultMediaQuery
    media <- liftIO $ getMedia repository query
    json media

getApiMediaById :: Repository -> ScottyM ()
getApiMediaById (Repository _ database) =
    get "/api/media/:id"
        (fromIntegral . read <$> param "id" >>= liftIO . getMediaById database
         >>= json)

defaultMediaQuery :: MediaQuery
defaultMediaQuery = MediaQuery { offset = 0, limit = 500 }

getMedia :: Repository -> MediaQuery -> IO [Media]
getMedia (Repository _ database) mediaQuery = catMaybes
    <$> getList' database (Query query noBind)
  where
    query = "@" ++ mediaCollection ++ "/* | desc /date desc /importDate"

getMediaById :: Database -> Int64 -> IO Media
getMediaById database id = getById database mediaCollection id
    >>= maybe (fail "media not found")
              (\media -> return $ media { Media.id = Just id })

