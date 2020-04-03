{-# LANGUAGE OverloadedStrings #-}

module Repository ( Repository(..), create, release ) where

import           Data.Text

import           Database.EJDB2 as EJDB2

import           Prelude        hiding ( init )

newtype Repository = Repository { database :: Database }

create :: Text -> IO Repository
create path = Repository <$> (EJDB2.init >> open options)
  where
    options = minimalOptions (unpack path) []

release :: Repository -> IO ()
release = close . database

