{-# LANGUAGE OverloadedStrings #-}

module Normalize where

import           Control.Monad.IO.Class

import           Data.Int

import qualified Database.EJDB2         as DB

import           Media

import           MediaMetadata

import           MediaThumbnails

import           Read

import           Repository

import           Update

import           Web.Scotty

postApiMediaNormalize :: Repository -> ScottyM ()
postApiMediaNormalize repository =
    post "/api/media/normalize" (liftIO $ normalizeAllMedia repository)

normalizeAllMedia :: Repository -> IO ()
normalizeAllMedia repository@(Repository homePath database) =
    DB.getList database (DB.Query "@media/*" DB.noBind)
    >>= mapM_ (normalizeMedia repository)

normalizeMedia :: Repository -> (Int64, Maybe Media) -> IO ()
normalizeMedia _ (_, Nothing) = return ()
normalizeMedia (Repository homePath database) (id, Just media) =
    updateMediaMetadata homePath mediaWithNormalizedFilePath
    >>= updateMediaThumbnails homePath
    >>= \m -> DB.put database mediaCollection m id
  where
    normalizedFilePath = fmap normalizeFilePath (Media.filePath media)

    mediaWithNormalizedFilePath = media { Media.filePath = normalizedFilePath }

normalizeFilePath :: String -> String
normalizeFilePath ('/' : path) = path
normalizeFilePath path = path

