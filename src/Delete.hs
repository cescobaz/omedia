{-# LANGUAGE OverloadedStrings #-}

module Delete ( deleteApiMedia ) where

import           Control.Exception
import           Control.Monad.IO.Class

import           Data.Int
import           Data.Text

import qualified Database.EJDB2            as EJDB2

import qualified File

import           Media

import           Network.HTTP.Types.Status

import           Prelude                   hiding ( id )

import           Read

import           Repository

import           System.Directory
import           System.FilePath

import           Web.Scotty

deleteApiMedia :: Repository -> ScottyM ()
deleteApiMedia repository = delete "/api/media/:id" $
    param "id" >>= liftIO . removeMedia repository >> status status204

removeMedia :: Repository -> Int64 -> IO ()
removeMedia (Repository homePath database) id =
    (getMediaById database id
     >>= maybe (return ()) (File.removeFileIfExists . (unpack homePath </>))
     . Media.filePath) >> EJDB2.delete database mediaCollection id
