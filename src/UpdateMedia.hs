{-# LANGUAGE OverloadedStrings #-}

module UpdateMedia where

import           Control.Monad
import           Control.Monad.IO.Class

import           Data.Int
import           Data.Maybe
import           Data.Text

import qualified Database.EJDB2         as DB

import           Media

import           ReadMedia

import           Repository

import           Web.Scotty

postApiMediaMetadata :: Repository -> ScottyM ()
postApiMediaMetadata repository = post "/api/media/:id/metadata" $
    (read <$> param "id") >>= liftIO . updateMetadata repository >>= json

updateMetadata :: Repository -> Int64 -> IO Media
updateMetadata (Repository homePath database) id =
    DB.getById database "media" id
    >>= maybe (fail "media not found")
              (\media ->
               maybe (fail "no filePath for media")
                     (\mediaFilePath -> do
                          let filePath = unpack homePath ++ mediaFilePath
                          metadata <- metadataFromFile filePath
                          let media' = media { Media.id = Just id
                                             , metadata = Just metadata
                                             , date     =
                                                   utcDateFromMetadata metadata
                                             }
                          DB.put database "media" media' id
                          return media')
                     (Media.filePath media))

