{-# LANGUAGE OverloadedStrings #-}

module DeleteMedia ( deleteApiMedia ) where

import           Control.Exception
import           Control.Monad.IO.Class

import           Data.Int

import qualified Database.EJDB2            as EJDB2

import qualified File

import           Media

import           Network.HTTP.Types.Status
import           Network.Multipart
import           Network.Wai.Handler.Warp  ( Port )

import           Prelude                   hiding ( id )

import           Repository

import           System.Directory

import           Web.Scotty

deleteApiMedia :: Repository -> ScottyM ()
deleteApiMedia repository = delete "/api/media/:id" $
    param "id" >>= liftIO . removeMedia repository >> status status204

removeMedia :: Repository -> Int64 -> IO ()
removeMedia (Repository homePath database) id = do
    media <- EJDB2.getById database "media" id
    case media of
        Nothing -> fail "media not found"
        Just media -> do
            File.removeFileIfExists $ Media.filePath media
            EJDB2.delete database "media" id
