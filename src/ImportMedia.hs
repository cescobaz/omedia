{-# LANGUAGE OverloadedStrings #-}

module ImportMedia ( postApiMedia ) where

import           Control.Monad.Fail       as M
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
    boundary <- boundary
    body <- body
    files <- liftIO $ parseBody boundary body homePath
    json files

boundary :: ActionM ST.Text
boundary = do
    contentType <- header "content-type"
    contentType <- justOrFail contentType "no content-type"
    contentType <- parseContentType $ LT.unpack contentType
    let boundary = L.find (\(key, _) -> key == "boundary")
                          (ctParameters contentType)
    (_, boundary) <- justOrFail boundary "missing boundary"
    return $ ST.pack boundary

parseBody :: ST.Text -> LB.ByteString -> ST.Text -> IO [ST.Text]
parseBody boundary body homePath = do
    let multipart@(MultiPart parts) =
            parseMultipartBody (ST.unpack boundary) body
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
            isHashEqual <- isHashEqual byteString filePath
            case (isHashEqual) of
                True -> return suggestedFilename
                False -> ImportMedia.writeFile homePath Nothing byteString

isHashEqual :: LB.ByteString -> FilePath -> IO Bool
isHashEqual byteString filePath = do
    fileData <- LB.readFile filePath
    let existingHash = hash fileData
    let incomingHash = hash byteString
    return (existingHash == incomingHash)

justOrFail :: MonadFail m => Maybe a -> String -> m a
justOrFail (Just value) _ = return value
justOrFail Nothing error = M.fail error
