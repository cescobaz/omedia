{-# LANGUAGE OverloadedStrings #-}

module Repository where

import           Data.Text

import           Database.SQLite3

import           Photo

newtype Repository = Repository { database :: Database }

create :: Text -> IO Repository
create path = do
    database <- open path
    return $ Repository database

release :: Repository -> IO ()
release repository = close $ database repository

getPhotos :: Repository -> IO [Photo]
getPhotos repository = do
    let database = Repository.database repository
    statement <- prepare database "select * from photos order by id desc"
    result <- step statement
    photos <- readPhotos statement result []
    finalize statement
    return photos

readPhotos :: Statement -> StepResult -> [Photo] -> IO [Photo]
readPhotos statement Done photos = return photos
readPhotos statement Row photos = do
    id <- columnInt64 statement 0
    filePath <- columnText statement 1
    let photo = Photo { Photo.id   = fromIntegral id
                      , filePath   = unpack filePath
                      , importDate = Nothing
                      , date       = Nothing
                      , tags       = []
                      }
    result <- step statement
    readPhotos statement result (photos ++ [ photo ])
