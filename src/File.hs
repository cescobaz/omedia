module File where

import           Control.Monad

import qualified Data.ByteString.Lazy as LB
import qualified Data.Hashable        as H
import           Data.UUID
import           Data.UUID.V4

import           System.Directory
import           System.FilePath

chooseFileName :: FilePath -> IO FilePath
chooseFileName filePath = do
    exists <- doesFileExist filePath
    if not exists
        then return filePath
        else do
            let directory = takeDirectory filePath
            let extension = takeExtension filePath
            createNotExistingFileName directory extension

createNotExistingFileName :: FilePath -> String -> IO FilePath
createNotExistingFileName directory extension = do
    filePath <- randomFileName directory extension
    exists <- doesFileExist filePath
    if not exists
        then return filePath
        else createNotExistingFileName directory extension

randomFileName :: FilePath -> String -> IO FilePath
randomFileName directory extension = do
    uuid <- nextRandom
    return $ directory </> Data.UUID.toString uuid ++ extension

hash :: FilePath -> IO Int
hash filePath = H.hash <$> LB.readFile filePath

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists filePath = do
    exists <- doesFileExist filePath
    when exists (removeFile filePath)

