module Autoimport where

import           Control.Concurrent ( threadDelay )

import qualified Import

import qualified ReadToImport       as RImport

import           Repository

autoimport :: Repository -> IO ()
autoimport repository = do
    files <- RImport.files repository
    putStrLn $ "autoimport on files " ++ show files
    results <- Import.importMedia repository files
    print results
    threadDelay 3000000
    autoimport repository

