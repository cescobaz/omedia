{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Import ( postApiMedia, fromFile ) where

import           Control.Exception
import           Control.Monad.IO.Class

import           Data.Aeson             ( ToJSON )
import           Data.Maybe
import           Data.Text

import           Database.EJDB2

import           File

import qualified Folders                as F

import           GHC.Generics

import           Media

import           MediaMetadata

import           MediaThumbnails

import           Prelude                hiding ( id )

import           Repository

import           System.Directory
import           System.FilePath

import           Time

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

    filePath = unpack homePath </> mediaToImport

importSingleFile :: Repository -> String -> IO (String, Maybe Media)
importSingleFile (Repository homePath database) filePath = do
    exists <- doesFileExist filePath
    if not exists
        then return ("file to import doesn't exists", Nothing)
        else do
            media
                <- catch (fromFile filePath)
                         ((\e -> print e
                           >> return (emptyMedia { Media.filePath =
                                                       Just filePath
                                                 })) :: IOException -> IO Media)
            let filename = takeFileName filePath
            let suggestedMediaFilePath =
                    unpack homePath </> F.media </> filename
            mediaFilePath
                <- chooseFileName suggestedMediaFilePath (File.hash filePath)
            case mediaFilePath of
                Nothing -> return ("already imported", Just media)
                Just mediaFilePath -> do
                    now <- Time.currentUTCDateTimeString
                    thumbnails <- createMediaThumbnails homePath filePath
                    let media' =
                            media { filePath   = Just $ F.media </> filename
                                  , importDate = Just now
                                  , date       =
                                        Just (fromMaybe now (date media))
                                  , thumbnails = Just thumbnails
                                  }
                    id <- putNew database mediaCollection media'
                    renameFile filePath mediaFilePath
                    return ( "ok"
                           , Just $ media' { id = Just $ fromIntegral id }
                           )

fromFile :: String -> IO Media
fromFile filePath = do
    metadata <- metadataFromFile filePath
    return $ (minimalMedia 0 filePath) { metadata = Just metadata
                                       , date     = utcDateFromMetadata metadata
                                       }
