{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ImportMedia ( postApiMedia, fromFile ) where

import           Control.Monad.IO.Class

import           CreateMediaThumbnails

import           Data.Aeson             ( ToJSON )
import           Data.Text
import qualified Data.Time.Clock        as Clock
import qualified Data.Time.Format       as Time

import           Database.EJDB2

import           File

import qualified Folders                as F

import           GHC.Generics

import           Media

import           MediaMetadata

import           Prelude                hiding ( id )

import           Repository

import           System.Directory
import           System.FilePath

import           Web.Scotty

data Result =
    Result { toImport :: String, result :: String, media :: Maybe Media }
    deriving ( Eq, Show, Generic )

instance ToJSON Result

postApiMedia :: Repository -> ScottyM ()
postApiMedia repository =
    post "/api/media/" (jsonData >>= liftIO . importMedia repository >>= json)

importMedia :: Repository -> [String] -> IO [Result]
importMedia repository = mapM (importSingleMedia repository)

importSingleMedia :: Repository -> String -> IO Result
importSingleMedia repository@(Repository homePath _) mediaToImport =
    if allowed
    then importSingleFile repository filePath >>= \(r, m) -> return $ res r m
    else return $ res "not allowed media" Nothing
  where
    res = Result mediaToImport

    allowed = "to-import/" `isPrefixOf` pack mediaToImport

    filePath = unpack homePath ++ mediaToImport

importSingleFile :: Repository -> String -> IO (String, Maybe Media)
importSingleFile (Repository homePath database) filePath = do
    exists <- doesFileExist filePath
    if not exists
        then return ("file to import doesn't exists", Nothing)
        else do
            media <- fromFile filePath
            let filename = takeFileName filePath
            let suggestedMediaFilePath =
                    unpack homePath </> F.media </> filename
            mediaFilePath
                <- chooseFileName suggestedMediaFilePath (File.hash filePath)
            case mediaFilePath of
                Nothing -> return ("already imported", Just media)
                Just mediaFilePath -> do
                    renameFile filePath mediaFilePath
                    date <- Time.formatTime Time.defaultTimeLocale
                                            "%Y-%m-%dT%T%QZ"
                        <$> Clock.getCurrentTime
                    thumbnails <- createMediaThumbnails homePath mediaFilePath
                    let media' =
                            media { filePath   = Just $ F.media </> filename
                                  , importDate = Just date
                                  , thumbnails = Just thumbnails
                                  }
                    id <- putNew database "media" media'
                    return ( "ok"
                           , Just $ media' { id = Just $ fromIntegral id }
                           )

fromFile :: String -> IO Media
fromFile filePath = do
    metadata <- metadataFromFile filePath
    return $ (minimalMedia 0 filePath) { metadata = Just metadata
                                       , date     = utcDateFromMetadata metadata
                                       }
