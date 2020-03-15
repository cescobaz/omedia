{-# LANGUAGE OverloadedStrings #-}

module Service where

import           Control.Monad.IO.Class

import           Data.Text

import           ImportMedia

import           Network.Wai.Handler.Warp             ( Port )
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static

import           ReadMedia

import           ReadToImport

import           Repository

import           Web.Scotty

data Options = Options { port :: Port, homePath :: Text }

run :: Service.Options -> IO ()
run options@(Service.Options port homePath) = do
    let databasePath = Data.Text.concat [ homePath, "/database.sqlite3" ]
    repository <- Repository.create databasePath
    scotty port $ do
        middleware logStdoutDev
        middleware $ staticPolicy $ resourcesPolicy homePath
        getApiMedia repository
        postApiMedia repository homePath
        getToImport repository homePath

resourcesPolicy :: Text -> Policy
resourcesPolicy homePath =
    (hasPrefix "media" <|> hasPrefix "thumbnails" <|> hasPrefix "to-import")
    >-> addBase (unpack homePath)



