{-# LANGUAGE OverloadedStrings #-}

module Repository ( Repository(..), create, release ) where

import           Data.Text

import           Database.SQLite3

newtype Repository = Repository { database :: Database }

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

