{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ImportMedia ( postApiMedia ) where

import qualified Codec.Picture               as P
import qualified Codec.Picture.Metadata      as M
import qualified Codec.Picture.Metadata.Exif as E

import           Control.Monad.IO.Class

import           Data.Aeson                  ( ToJSON )
import           Data.Text

import qualified Folders                     as F

import           GHC.Generics

import           Media

import           Repository

import           Web.Scotty

data Result =
    Result { toImport :: String, result :: String, media :: Maybe Media }
    deriving ( Eq, Show, Generic )

instance ToJSON Result

postApiMedia :: Repository -> Text -> ScottyM ()
postApiMedia repository homePath =
    post "/api/media/" (jsonData >>= (liftIO . importMedia homePath) >>= json)

importMedia :: Text -> [String] -> IO [Result]
importMedia homePath = mapM (importSingleMedia homePath)

importSingleMedia :: Text -> String -> IO Result
importSingleMedia homePath mediaToImport = do
    result <- P.readImageWithMetadata filePath
    case result of
        Left message -> return Result { toImport = mediaToImport
                                      , result   = message
                                      , media    = Nothing
                                      }
        Right (_, metadatas) -> importMediaWithMetadatas filePath metadatas
  where
    filePath =
        unpack homePath ++ F.surroundWithSlashes F.toImport ++ mediaToImport

importMediaWithMetadatas :: FilePath -> M.Metadatas -> IO Result
importMediaWithMetadatas filePath metadatas = do
    let dateTimeOriginal = M.lookup (M.Exif $ E.TagUnknown 0x9003) metadatas
    return Result {  }
