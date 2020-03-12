{-# LANGUAGE OverloadedStrings #-}

module Repository
    ( Repository
    , create
    , release
    , MediaQuery(..)
    , defaultMediaQuery
    , getMedia
    ) where

import           Data.Text

import           Database.SQLite3

import           Media

newtype Repository = Repository { database :: Database }

data MediaQuery = MediaQuery { offset :: Int, limit :: Int }

create :: Text -> IO Repository
create path = do
    database <- open path
    let initScript = "create table if not exists photos (\
                     \ id integer primary key autoincrement,\
                     \ file_path text not null unique\
                     \ );\
                     \ "
    exec database initScript
    return $ Repository database

release :: Repository -> IO ()
release = close . database

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
