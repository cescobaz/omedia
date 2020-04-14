{-# LANGUAGE OverloadedStrings #-}

module Service where

import           Control.Monad.IO.Class

import           Data.Text

import           Delete

import           Import

import           MediaMetadata

import           MediaThumbnails

import           Network.Wai.Handler.Warp             ( Port )
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static

import           Normalize

import           Read

import           ReadToImport

import           Repository

import           Upload

import           Web.Scotty

data Options = Options { port :: Port, homePath :: Text }

run :: Service.Options -> IO ()
run options@(Service.Options port homePath) = do
    let databasePath = Data.Text.concat [ homePath, "/database.ejdb" ]
    repository <- Repository.create homePath databasePath
    scotty port $ do
        middleware logStdoutDev
        middleware $ staticPolicy $ resourcesPolicy homePath
        postApiMediaNormalize repository
        getApiMedia repository
        getApiMediaById repository
        postToImport repository
        getToImport repository
        postApiMedia repository
        deleteApiMedia repository
        postApiMediaMetadata repository
        postApiMediaThumbnails repository

resourcesPolicy :: Text -> Policy
resourcesPolicy homePath = (hasPrefix "media/" <|> hasPrefix "media-thumbnails/"
                            <|> hasPrefix "to-import/")
    >-> addBase (unpack homePath)



