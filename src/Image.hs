{-# LANGUAGE ForeignFunctionInterface #-}

module Image ( scale ) where

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

foreign import ccall unsafe "oimage.h thumbnail" c_thumbnail
        :: CString -> CString -> CInt -> Ptr CInt -> Ptr CInt -> IO CInt

checkRC :: String -> CInt -> IO ()
checkRC _ 0 = return ()
checkRC function resultCode =
    fail ("Error: Module Image: " ++ function ++ " -> " ++ show resultCode)

scale :: String -> String -> Int -> IO (Int, Int)
scale ifile ofile maxSize = withCString ifile $ \cifile ->
    withCString ofile $ \cofile -> alloca $ \widthPtr -> alloca $ \heightPtr ->
    c_thumbnail cifile cofile (fromIntegral maxSize) widthPtr heightPtr
    >>= checkRC "scale" >> peek widthPtr >>= \width -> peek heightPtr
    >>= \height -> return (fromIntegral width, fromIntegral height)
