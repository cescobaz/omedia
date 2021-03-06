{-# LANGUAGE OverloadedStrings #-}

module Service where

import qualified Autoimport

import           Control.Concurrent                   ( forkIO )

import           Data.Text

import qualified Database.EJDB2                       as Database

import           Delete

import           Import

import           MediaMetadata

import           MediaThumbnails

import           Network.Wai.Handler.Warp             ( Port )
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static

import           Normalize

import           Read

import           ReadTag

import           ReadToImport

import           Repository

import           Tag

import           Upload

import           Web.Scotty

data Options = Options { port :: Port, homePath :: Text }

run :: Service.Options -> IO ()
run options@(Service.Options port homePath) = do
    let databasePath = Data.Text.concat [ homePath, "/database.ejdb" ]
    repository <- Repository.create homePath databasePath
    Database.ensureIndex (database repository)
                         mediaCollection
                         "/date /importDate"
                         [ Database.strIndexMode ]
    forkIO (Autoimport.autoimport repository)
    scotty port $ do
        middleware logStdoutDev
        middleware $ staticPolicy $ resourcesPolicy homePath
        postApiMediaBulkTags repository
        deleteApiMediaTagsBulk repository
        getApiMediaTags repository
        postApiMediaNormalize repository
        getApiMedia repository
        getApiMediaById repository
        postToImport repository
        getToImport repository
        postApiMedia repository
        deleteApiMedia repository
        postApiMediaMetadata repository
        postApiMediaThumbnails repository
        postApiMediaTags repository

resourcesPolicy :: Text -> Policy
resourcesPolicy homePath = (hasPrefix "media/" <|> hasPrefix "media-thumbnails/"
                            <|> hasPrefix "to-import/")
    >-> addBase (unpack homePath)



