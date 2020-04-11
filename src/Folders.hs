module Folders where

addBeginningSlash :: String -> String
addBeginningSlash folder = "/" ++ folder

addEndingSlash :: String -> String
addEndingSlash folder = folder ++ "/"

surroundWithSlashes :: String -> String
surroundWithSlashes = addBeginningSlash . addEndingSlash

toImport :: String
toImport = "to-import"

media :: String
media = "media"

mediaThumbnails :: String
mediaThumbnails = "media-thumbnails"
