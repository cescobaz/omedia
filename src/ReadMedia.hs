{-# LANGUAGE OverloadedStrings #-}

module ReadMedia ( getApiMedia ) where

import           Control.Monad.IO.Class

import           Data.Aeson                           ( FromJSON, ToJSON )
import           Data.Text
import           Data.Text

import           Database.SQLite3

import           Media
import           Media

import           Network.Wai.Handler.Warp             ( Port )
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static

import           Repository

import           Web.Scotty

data MediaQuery = MediaQuery { offset :: Int, limit :: Int }

instance ToJSON Media

getApiMedia :: Repository -> ScottyM ()
getApiMedia repository = get "/api/media/" $ do
    let query = defaultMediaQuery
    media <- liftIO $ getMedia repository query
    json media

defaultMediaQuery :: MediaQuery
defaultMediaQuery = MediaQuery { offset = 0, limit = 25 }

getMedia :: Repository -> MediaQuery -> IO [Media]
getMedia repository query = do
    let database = Repository.database repository
    let sQuery =
            "select id, file_path from photos order by id desc limit ? offset ?"
    statement <- prepare database sQuery
    bindInt statement 1 (limit query)
    bindInt statement 2 (offset query)
    result <- step statement
    photos <- readMedia statement result []
    finalize statement
    return photos

readMedia :: Statement -> StepResult -> [Media] -> IO [Media]
readMedia statement Done photos = return photos
readMedia statement Row photos = do
    id <- columnInt64 statement 0
    filePath <- columnText statement 1
    let photo = Media { Media.id   = fromIntegral id
                      , filePath   = unpack filePath
                      , importDate = Nothing
                      , date       = Nothing
                      , tags       = []
                      }
    result <- step statement
    readMedia statement result (photos ++ [ photo ])

