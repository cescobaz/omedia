{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ImportMedia ( postApiMedia ) where

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

import           Network.Multipart
import           Network.Wai.Handler.Warp ( Port )

import           Repository

import           System.Directory

import           Web.Scotty

data Result = Result { result      :: ST.Text
                     , name        :: Maybe ST.Text
                     , filename    :: Maybe ST.Text
                     , contentType :: Maybe ST.Text
                     , path        :: Maybe ST.Text
                     }
    deriving ( Eq, Show, Generic )

instance ToJSON Result

postApiMedia :: Repository -> ST.Text -> ScottyM ()
postApiMedia repository homePath = post "/api/media/" $ do
    boundary <- boundary
    body <- body
    results <- liftIO $ parseBody boundary body homePath
    json results

boundary :: ActionM ST.Text
boundary = do
    contentType <- header "content-type"
    contentType <- justOrFail contentType "no content-type"
    contentType <- parseContentType $ LT.unpack contentType
    let boundary = L.find (\(key, _) -> key == "boundary")
                          (ctParameters contentType)
    (_, boundary) <- justOrFail boundary "missing boundary"
    return $ ST.pack boundary

parseBody :: ST.Text -> LB.ByteString -> ST.Text -> IO [Result]
parseBody boundary body homePath = do
    let multipart = parseMultipartBody (ST.unpack boundary) body
    parseBodyParts homePath multipart

parseBodyParts :: ST.Text -> MultiPart -> IO [Result]
parseBodyParts homePath (MultiPart bodyParts) =
    mapM (parseBodyPart homePath) bodyParts

findValue :: String -> [(String, String)] -> Maybe String
findValue key = (fmap snd) . (L.find (\(k, _) -> k == key))

parseBodyPart :: ST.Text -> BodyPart -> IO Result
parseBodyPart homePath (BodyPart headers byteString) = do
    contentType <- getContentType headers
    let contentType' = ctType contentType
    contentDisposition <- getContentDisposition headers
    let ContentDisposition _ attributes = contentDisposition
    let name = findValue "name" attributes
    let filename = findValue "filename" attributes
    result <- ImportMedia.writeFile homePath filename byteString
    return result { name        = fmap ST.pack name
                  , filename    = fmap ST.pack filename
                  , contentType = Just $ ST.pack contentType'
                  }

writeFile :: ST.Text -> Maybe String -> LB.ByteString -> IO Result
writeFile homePath Nothing byteString = do
    uuid <- nextRandom
    ImportMedia.writeFile homePath (Just $ Data.UUID.toString uuid) byteString
writeFile homePath (Just suggestedFilename) byteString = do
    let path = "/to-import/" ++ suggestedFilename
    let filePath = (ST.unpack homePath) ++ path
    exists <- doesFileExist filePath
    case exists of
        False -> do
            LB.writeFile filePath byteString
            return Result { name        = Nothing
                          , filename    = Nothing
                          , path        = Just $ ST.pack path
                          , contentType = Nothing
                          , result      = "ok"
                          }
        True -> do
            isHashEqual <- isHashEqual byteString filePath
            case (isHashEqual) of
                True -> return Result { name        = Nothing
                                      , filename    = Nothing
                                      , path        = Just $ ST.pack path
                                      , result      = "skipped because exists"
                                      , contentType = Nothing
                                      }
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
