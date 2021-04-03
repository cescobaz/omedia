{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Image where

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

foreign import ccall unsafe "oimage.h thumbnails" c_thumbnail
        :: CString -> CString -> CInt -> IO CInt

scale :: String -> String -> Int -> IO Int
scale ifile ofile maxSize = fromIntegral
    <$> withCString ifile
                    (\cifile ->
                     withCString ofile
                                 (\cofile -> c_thumbnail cifile
                                                         cofile
                                                         (fromIntegral maxSize)))
