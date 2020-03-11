{-# LANGUAGE DeriveGeneric #-}

module Photo where

import           GHC.Generics

data Photo = Photo { id         :: Int
                   , filePath   :: String
                   , importDate :: Maybe String
                   , date       :: Maybe String
                   , tags       :: [String]
                   }
    deriving ( Eq, Show, Generic )
