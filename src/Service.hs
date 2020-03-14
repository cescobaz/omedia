{-# LANGUAGE OverloadedStrings #-}

module Service where

import           Control.Monad.IO.Class

import           Data.Aeson                           ( FromJSON, ToJSON )
import           Data.Text

import           Media

import           Network.Wai.Handler.Warp             ( Port )
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static

import           Repository

import           Web.Scotty

instance ToJSON Media

data Options = Options { port :: Port, homePath :: Text }

run :: Service.Options -> IO ()
run options@(Service.Options port homePath) = do
    let databasePath = Data.Text.concat [ homePath, "/database.sqlite3" ]
    repository <- Repository.create databasePath
    scotty port $ do
        middleware logStdoutDev
        middleware $ staticPolicy $ resourcesPolicy homePath
        get "/api/media/" $ do
            let query = Repository.defaultMediaQuery
            media <- liftIO $ Repository.getMedia repository query
            json media

resourcesPolicy :: Text -> Policy
resourcesPolicy homePath = ((hasPrefix "media") <|> (hasPrefix "thumbnails")
                            <|> (hasPrefix "to-import"))
    >-> (addBase $ unpack homePath)



