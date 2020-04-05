{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module UploadMedia ( postToImport ) where

import           Control.Exception        as E
import           Control.Monad.Fail       as M
import           Control.Monad.IO.Class

import           Data.Aeson               ( ToJSON )
import           Data.ByteString          as B
import           Data.ByteString.Lazy     as LB
import           Data.Hashable
import           Data.List                as L
import           Data.Text                as ST
import           Data.Text.Lazy           as LT
import           Data.UUID
import           Data.UUID.V4

import           GHC.Generics

import           HLib

import           Media

import           Network.Multipart
import           Network.Wai.Handler.Warp ( Port )

import           Repository

import           System.Directory
import           System.FilePath.Posix

import           Web.Scotty

data Result = Result { result      :: ST.Text
                     , name        :: Maybe ST.Text
                     , filename    :: Maybe ST.Text
                     , contentType :: Maybe ST.Text
                     , path        :: Maybe ST.Text
                     }
    deriving ( Eq, Show, Generic )

instance ToJSON Result

postToImport :: Repository -> ScottyM ()
postToImport (Repository homePath _) = post "/to-import/" $ do
    boundary <- boundary
    body <- body
    results <- liftIO $ parseBody boundary body homePath
    json results

boundary :: ActionM ST.Text
boundary = do
    contentType <- header "content-type"
    contentType <- justOrFail contentType "no content-type"
    contentType <- parseContentType $ LT.unpack contentType
    let boundary = findValue "boundary" (ctParameters contentType)
    boundary <- justOrFail boundary "missing boundary"
    return $ ST.pack boundary

parseBody :: ST.Text -> LB.ByteString -> ST.Text -> IO [Result]
parseBody boundary body homePath = do
    let multipart = parseMultipartBody (ST.unpack boundary) body
    parseBodyParts homePath multipart

parseBodyParts :: ST.Text -> MultiPart -> IO [Result]
parseBodyParts homePath (MultiPart bodyParts) =
    mapM (parseBodyPart homePath) bodyParts

findValue :: String -> [(String, String)] -> Maybe String
findValue key = fmap snd . L.find (\(k, _) -> k == key)

checkContentType :: Headers -> IO String
checkContentType headers = do
    contentType <- getContentType headers
    let contentType' = ctType contentType
    if isContentTypeAllowed contentType'
        then return contentType'
        else M.fail $ "content type not supported: " ++ contentType'

parseBodyPart :: ST.Text -> BodyPart -> IO Result
parseBodyPart homePath (BodyPart headers byteString) = do
    contentDisposition <- getContentDisposition headers
    let ContentDisposition _ attributes = contentDisposition
    let name = findValue "name" attributes
    let filename = findValue "filename" attributes
    catch (parseContentTypeAndWrite homePath headers byteString name filename)
          (handleException name filename)

parseContentTypeAndWrite
    :: ST.Text
    -> Headers
    -> LB.ByteString
    -> Maybe String
    -> Maybe String
    -> IO Result
parseContentTypeAndWrite homePath headers byteString name filename = do
    contentType <- checkContentType headers
    (result, path)
        <- UploadMedia.writeFile homePath byteString filename Nothing
    return Result { name        = fmap ST.pack name
                  , filename    = fmap ST.pack filename
                  , contentType = Just $ ST.pack contentType
                  , path        = Just path
                  , result      = result
                  }

handleException :: Maybe String -> Maybe String -> IOException -> IO Result
handleException name filename exception =
    return Result { name        = fmap ST.pack name
                  , filename    = fmap ST.pack filename
                  , contentType = Nothing
                  , path        = Nothing
                  , result      = ST.pack $ displayException exception
                  }

writeFile :: ST.Text
          -> LB.ByteString
          -> Maybe String
          -> Maybe String
          -> IO (ST.Text, ST.Text)
writeFile homePath byteString Nothing Nothing = do
    uuid <- nextRandom
    UploadMedia.writeFile homePath
                          byteString
                          (Just $ Data.UUID.toString uuid)
                          Nothing
writeFile homePath byteString Nothing (Just extension) = do
    uuid <- nextRandom
    UploadMedia.writeFile homePath
                          byteString
                          (Just $ Data.UUID.toString uuid ++ extension)
                          Nothing
writeFile homePath byteString (Just suggestedFilename) _ = do
    let path = "/to-import/" ++ suggestedFilename
    let filePath = ST.unpack homePath ++ path
    exists <- doesFileExist filePath
    if not exists
        then do
            LB.writeFile filePath byteString
            return ("ok", ST.pack path)
        else do
            isHashEqual <- isHashEqual byteString filePath
            if isHashEqual
                then return ("skipped because exists", ST.pack path)
                else UploadMedia.writeFile homePath
                                           byteString
                                           Nothing
                                           (Just $
                                            takeExtension suggestedFilename)

isHashEqual :: LB.ByteString -> FilePath -> IO Bool
isHashEqual byteString filePath = do
    fileData <- LB.readFile filePath
    let existingHash = hash fileData
    let incomingHash = hash byteString
    return (existingHash == incomingHash)
