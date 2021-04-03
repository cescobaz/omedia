{-# LANGUAGE ForeignFunctionInterface #-}

module Image ( scale ) where

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

foreign import ccall unsafe "oimage.h thumbnail" c_thumbnail
        :: CString -> CString -> CInt -> IO CInt

checkRC :: String -> CInt -> IO ()
checkRC _ 0 = return ()
checkRC function resultCode =
    fail ("Error: Module Image: " ++ function ++ " -> " ++ show resultCode)

scale :: String -> String -> Int -> IO ()
scale ifile ofile maxSize =
    withCString ifile
                (\cifile ->
                 withCString ofile
                             (\cofile ->
                              c_thumbnail cifile cofile (fromIntegral maxSize)
                              >>= checkRC "scale"))
