{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ImportMedia ( postApiMedia ) where

import           Control.Monad.IO.Class

import           Data.Aeson             ( ToJSON )
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
    post "/api/media/" (jsonData >>= (liftIO . importMedia homePath) >>= json)

importMedia :: Text -> [String] -> IO [Result]
importMedia homePath = mapM (importSingleMedia homePath)

importSingleMedia :: Text -> String -> IO Result
importSingleMedia homePath mediaToImport = do
    media <- Media.fromFile filePath
    return Result { media = Just media }
  where
    filePath =
        unpack homePath ++ F.surroundWithSlashes F.toImport ++ mediaToImport
