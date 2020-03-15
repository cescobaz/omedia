{-# LANGUAGE OverloadedStrings #-}

module ReadToImport ( getToImport ) where

import           Control.Monad.IO.Class

import           Data.Aeson             ( ToJSON )
import           Data.List
import           Data.Text              as ST

import           Media

import           Repository

import           System.Directory

import           Web.Scotty

instance ToJSON Media

getToImport :: Repository -> ST.Text -> ScottyM ()
getToImport repository homePath = get "/api/to-import/" $ do
    files <- liftIO $ ReadToImport.files ((ST.unpack homePath) ++ "/to-import")
    json files

files :: FilePath -> IO [Media]
files path = do
    entries <- listDirectory path
    return $ Data.List.map ReadToImport.map $
        Data.List.filter (isSuffixAllowed . extension) entries

map :: FilePath -> Media
map filePath = Media { Media.id   = 0
                     , filePath   = "/to-import/" ++ filePath
                     , importDate = Nothing
                     , date       = Nothing
                     , tags       = []
                     }

extension :: FilePath -> String
extension path = unpack e
  where
    (_, e) = ST.breakOnEnd "." (pack path)

isSuffixAllowed :: String -> Bool
isSuffixAllowed "jpeg" = True
isSuffixAllowed "jpg" = True
isSuffixAllowed "png" = True
isSuffixAllowed _ = False
