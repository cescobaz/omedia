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

data Options = Options { port           :: Port
                       , databasePath   :: Text
                       , mediaPath      :: Text
                       , thumbnailsPath :: Text
                       , toImportPath   :: Text
                       }

run :: Service.Options -> IO ()
run options@(Service.Options port
                             databasePath
                             mediaPath
                             thumbnailsPath
                             toImportPath) = do
    repository <- Repository.create databasePath
    scotty port $ do
        middleware logStdoutDev
        get "/-/ready" $ json ([ "ciao" ] :: [String])
        get "/api/media/" $ do
            let query = Repository.defaultMediaQuery
            media <- liftIO $ Repository.getMedia repository query
            json media
        middleware $ staticPolicy (policy $ rewrite options)

rewrite :: Service.Options -> (String -> Maybe String)
rewrite options path = rewritePath options $ Data.Text.breakOn "/" (pack path)

rewritePath :: Service.Options -> (Text, Text) -> Maybe String
rewritePath options (prefix, path) = createPath options (basePath prefix) path

createPath :: Service.Options
           -> (Maybe (Service.Options -> Text))
           -> Text
           -> Maybe String
createPath _ Nothing _ = Nothing
createPath options (Just getBasePath) path = Just $ unpack $
    Data.Text.concat [ (getBasePath options), "/", path ]

basePath :: Text -> Maybe (Service.Options -> Text)
basePath "media" = Just mediaPath
basePath "thumbnails" = Just thumbnailsPath
basePath "to-import" = Just toImportPath
basePath _ = Nothing


