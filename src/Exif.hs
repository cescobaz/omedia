{-# LANGUAGE ForeignFunctionInterface #-}

module Exif where

import           Control.Exception

import qualified Data.Map.Strict   as Map

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Storable

foreign import ccall unsafe "oexif.h exif" c_exif
        :: CString -> Ptr (Ptr (Ptr ())) -> Ptr CInt -> IO CInt

foreign import ccall unsafe "oexif.h free_exif" c_free_exif
        :: Ptr (Ptr (Ptr ())) -> CInt -> IO ()

checkRC :: String -> CInt -> IO ()
checkRC _ 0 = return ()
checkRC function resultCode =
    fail ("Error: Module Exif: " ++ function ++ " -> " ++ show resultCode)

type Value = Maybe String

exif :: FilePath -> IO (Map.Map String Value)
exif filePath =
    withCString filePath
                (\cFilePath ->
                 alloca (\intPtr ->
                         alloca (\ptr -> do
                                     c_exif cFilePath ptr intPtr
                                         >>= checkRC "exif"
                                     metadata <- peek ptr
                                     count <- peek intPtr
                                     finally (parse metadata
                                                    (fromIntegral count))
                                             (c_free_exif ptr count))))

parse :: Ptr (Ptr ()) -> Int -> IO (Map.Map String Value)
parse _ 0 = return Map.empty
parse ptr 1 = uncurry Map.singleton <$> parseKeyValue ptr
parse ptr count = do
    (k, v) <- parseKeyValue ptr
    Map.insert k v <$> parse (nextKeyValue ptr) (count - 1)

parseKeyValue :: Ptr (Ptr ()) -> IO (String, Value)
parseKeyValue ptr = do
    key <- peek ptr >>= peekCString . castPtr
    value <- peek (plusPtr ptr step) >>= \valuePtr ->
        if valuePtr == nullPtr
        then return Nothing
        else Just <$> peekCString (castPtr valuePtr)
    return (key, value)
  where
    step = sizeOf ptr

nextKeyValue :: Ptr (Ptr ()) -> Ptr (Ptr ())
nextKeyValue ptr = plusPtr (plusPtr ptr step) step
  where
    step = sizeOf ptr

