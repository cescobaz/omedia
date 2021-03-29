{-# LANGUAGE OverloadedStrings #-}

module ReadToImport ( getToImport, media, ReadToImport.files ) where

import           Control.Monad.IO.Class

import           Data.List
import           Data.Text              as ST

import           Media

import           Repository

import           System.Directory
import           System.FilePath

import           Web.Scotty

getToImport :: Repository -> ScottyM ()
getToImport repository =
    get "/api/to-import/" (liftIO (ReadToImport.media repository) >>= json)

media :: Repository -> IO [Media]
media repository = fmap ReadToImport.map <$> ReadToImport.files repository

files :: Repository -> IO [FilePath]
files (Repository homePath _) = Data.List.map ("to-import/" </>)
    . Data.List.filter (Media.isSuffixAllowed . takeExtension)
    <$> listDirectory (ST.unpack homePath ++ "/to-import")

map :: FilePath -> Media
map = minimalMedia 0
