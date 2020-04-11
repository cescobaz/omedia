{-# LANGUAGE OverloadedStrings #-}

module ReadToImport ( getToImport ) where

import           Control.Monad.IO.Class

import           Data.List
import           Data.Text              as ST

import           Media

import           Repository

import           System.Directory
import           System.FilePath

import           Web.Scotty

getToImport :: Repository -> ScottyM ()
getToImport (Repository homePath _) = get "/api/to-import/" $ do
    files <- liftIO $ ReadToImport.files (ST.unpack homePath ++ "/to-import")
    json files

files :: FilePath -> IO [Media]
files path = do
    entries <- listDirectory path
    return $ Data.List.map ReadToImport.map $
        Data.List.filter (Media.isSuffixAllowed . takeExtension) entries

map :: FilePath -> Media
map filePath = minimalMedia 0 ("to-import/" </> filePath)
