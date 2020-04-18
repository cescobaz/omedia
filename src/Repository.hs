{-# LANGUAGE OverloadedStrings #-}

module Repository ( Repository(..), create, release, mediaCollection ) where

import           Data.Text

import           Database.EJDB2 as EJDB2

import           Prelude        hiding ( init )

data Repository = Repository { homePath :: Text, database :: Database }

create :: Text -> Text -> IO Repository
create homePath path = Repository homePath <$> (EJDB2.init >> open options)
  where
    options = minimalOptions (unpack path) []

release :: Repository -> IO ()
release = close . database

mediaCollection :: String
mediaCollection = "media"
