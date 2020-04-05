{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ImportMedia ( postApiMedia ) where

import           Control.Monad.IO.Class

import           Data.Aeson             ( ToJSON )
import           Data.Text
import           Data.Text

import qualified Folders                as F

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
    post "/api/media/"
         (jsonData >>= (liftIO . importMedia repository homePath) >>= json)

importMedia :: Repository -> Text -> [String] -> IO [Result]
importMedia repository homePath = mapM (importSingleMedia repository homePath)

importSingleMedia :: Repository -> Text -> String -> IO Result
importSingleMedia repository homePath mediaToImport = do
    if allowed
        then (importSingleFile repository filePath
              >>= \(r, m) -> return $ res r m)
        else (return $ res "not allowed media" Nothing)
  where
    res = Result mediaToImport

    allowed = isPrefixOf "/to-import/" (pack mediaToImport)

    filePath = (unpack homePath) ++ mediaToImport

importSingleFile :: Repository -> String -> IO (String, Maybe Media)
importSingleFile repository filePath = do
    media <- Media.fromFile filePath

    return ("boh", Just media)
