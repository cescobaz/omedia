module File where

import           Control.Monad

import qualified Data.ByteString.Lazy  as LB
import qualified Data.Hashable         as H
import           Data.UUID
import           Data.UUID.V4

import           System.Directory
import           System.FilePath.Posix

chooseFileName :: FilePath -> IO Int -> IO (Maybe FilePath)
chooseFileName filePath hashIO = do
    exists <- doesFileExist filePath
    if not exists
        then return $ Just filePath
        else do
            iHash <- hashIO
            fileHash <- hash filePath
            if fileHash == iHash
                then return Nothing
                else do
                    let directory = takeDirectory filePath
                    let extension = takeExtension filePath
                    randomFilePath <- randomFileName directory extension
                    chooseFileName randomFilePath hashIO

chooseNotExistingFileName :: FilePath -> String -> IO FilePath
chooseNotExistingFileName directory extension = do
    filePath <- randomFileName directory extension
    exists <- doesFileExist filePath
    if not exists
        then return filePath
        else chooseNotExistingFileName directory extension

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

