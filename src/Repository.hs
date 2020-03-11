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
getPhotos repository = return []
