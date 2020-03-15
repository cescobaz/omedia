{-# LANGUAGE OverloadedStrings #-}

module ImportMedia ( postApiMedia ) where

import           Control.Monad.IO.Class

import           Data.ByteString          as B
import           Data.ByteString.Lazy     as LB
import           Data.Hashable
import           Data.List                as L
import           Data.Text                as ST
import           Data.Text.Lazy           as LT
import           Data.UUID
import           Data.UUID.V4

import           Network.Multipart
import           Network.Wai.Handler.Warp ( Port )

import           Repository

import           System.Directory

import           Web.Scotty

postApiMedia :: Repository -> ST.Text -> ScottyM ()
postApiMedia repository homePath = post "/api/media/" $ do
    contentType <- header "content-type"
    case contentType of
        Nothing -> raise "no content-type"
        Just contentType -> do
            contentType <- parseContentType $ LT.unpack contentType
            let boundary = L.find (\(key, _) -> key == "boundary")
                                  (ctParameters contentType)
            case boundary of
                Nothing -> raise "missing boundary"
                Just (_, boundary) -> do
                    body <- body
                    files <- liftIO $ parseBody boundary body homePath
                    json files

parseBody :: String -> LB.ByteString -> ST.Text -> IO [ST.Text]
parseBody boundary body homePath = do
    let multipart@(MultiPart parts) = parseMultipartBody boundary body
    parseBodyParts homePath multipart

parseBodyParts :: ST.Text -> MultiPart -> IO [ST.Text]
parseBodyParts homePath (MultiPart bodyParts) =
    mapM (parseBodyPart homePath) bodyParts

parseBodyPart :: ST.Text -> BodyPart -> IO ST.Text
parseBodyPart homePath (BodyPart headers byteString) = do
    contentDisposition <- getContentDisposition headers
    let ContentDisposition _ attributes = contentDisposition
    let filenameAttribute = L.find (\(key, _) -> key == "filename") attributes
    let filenameValue = case filenameAttribute of
            Nothing -> Nothing
            Just (_, value) -> Just value
    filename <- ImportMedia.writeFile homePath filenameValue byteString
    return (ST.pack filename)

writeFile :: ST.Text -> Maybe String -> LB.ByteString -> IO FilePath
writeFile homePath Nothing byteString = do
    uuid <- nextRandom
    ImportMedia.writeFile homePath (Just $ Data.UUID.toString uuid) byteString
writeFile homePath (Just suggestedFilename) byteString = do
    let filePath = (ST.unpack homePath) ++ "/to-import/" ++ suggestedFilename
    exists <- doesFileExist filePath
    case exists of
        False -> do
            LB.writeFile filePath byteString
            return suggestedFilename
        True -> do
            fileData <- LB.readFile filePath
            let existingHash = hash fileData
            let incomingHash = hash byteString
            case (existingHash == incomingHash) of
                True -> do
                    LB.writeFile filePath byteString
                    return suggestedFilename
                False -> ImportMedia.writeFile homePath Nothing byteString

