{-# LANGUAGE DeriveGeneric #-}

module Media ( Media(..) ) where

import           GHC.Generics

data Media = Media { id         :: Int
                   , filePath   :: String
                   , importDate :: Maybe String
                   , date       :: Maybe String
                   , tags       :: [String]
                   }
    deriving ( Eq, Show, Generic )
