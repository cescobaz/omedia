{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Service

main :: IO ()
main =
    Service.run Options { port           = 3000
                        , databasePath   =
                              "/Users/cescobaz/omedia/database.sqlite3"
                        , mediaPath      = "/Users/cescobaz/omedia/media"
                        , thumbnailsPath = "/Users/cescobaz/omedia/thumbnails"
                        , toImportPath   = "/Users/cescobaz/omedia/to-import"
                        }
