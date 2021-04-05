{-# LANGUAGE ForeignFunctionInterface #-}

module Exif ( exif ) where

import           Binding.Metadata

import           Control.Exception

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

import           Media             ( Metadata(..) )

foreign import ccall unsafe "oexif.h exif" c_exif
        :: CString -> Ptr (Ptr Metadata) -> IO CInt

foreign import ccall unsafe "oexif.h free_exif" c_free_exif
        :: Ptr (Ptr Metadata) -> IO ()

checkRC :: String -> CInt -> IO ()
checkRC _ 0 = return ()
checkRC function resultCode =
    fail ("Error: Module Exif: " ++ function ++ " -> " ++ show resultCode)

exif :: FilePath -> IO Metadata
exif filePath =
    withCString filePath
                (\cFilePath ->
                 alloca (\ptr -> do
                             c_exif cFilePath ptr >>= checkRC "exif"
                             finally (peek ptr >>= peek) (c_free_exif ptr)))

