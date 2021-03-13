{-# LANGUAGE OverloadedStrings #-}

module Normalize where

import           Control.Exception
import           Control.Monad.IO.Class

import           Data.Int
import           Data.Maybe

import qualified Database.EJDB2         as DB

import           Media

import           MediaMetadata

import           MediaThumbnails

import           Repository

import           Web.Scotty

postApiMediaNormalize :: Repository -> ScottyM ()
postApiMediaNormalize repository =
    post "/api/media/normalize" (liftIO $ normalizeAllMedia repository)

normalizeAllMedia :: Repository -> IO ()
normalizeAllMedia repository@(Repository _ database) =
    DB.getList database (DB.Query "@media/*" DB.noBind)
    >>= mapM_ (\m -> catch (normalizeMedia repository m
                            >> putStrLn (show (Media.filePath <$> snd m)
                                         ++ " normalized"))
                           (print :: IOException -> IO ()))

catchUpdateMedia :: Media -> IOException -> IO Media
catchUpdateMedia m e = print e >> return m

normalizeMedia :: Repository -> (Int64, Maybe Media) -> IO ()
normalizeMedia _ (_, Nothing) = return ()
normalizeMedia (Repository homePath database) (id, Just media) =
    catch (updateMediaMetadata homePath mediaWithNormalizedFilePath)
          (catchUpdateMedia media) >>= \m ->
    catch (updateMediaThumbnails homePath m) (catchUpdateMedia m) >>= \m ->
    DB.put database mediaCollection (normalizeMediaDate m) id
  where
    normalizedFilePath = fmap normalizeFilePath (Media.filePath media)

    mediaWithNormalizedFilePath = media { Media.filePath = normalizedFilePath }

normalizeFilePath :: String -> String
normalizeFilePath ('/' : path) = path
normalizeFilePath path = path

normalizeMediaDate :: Media -> Media
normalizeMediaDate media
    | isJust (Media.date media) = media
    | otherwise = media { Media.date = Media.importDate media }
