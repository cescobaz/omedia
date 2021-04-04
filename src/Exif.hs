{-# LANGUAGE ForeignFunctionInterface #-}

module Exif where

import           Control.Exception
import           Control.Monad

import qualified Data.Map.Strict   as Map
import           Data.Ratio

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Storable

import           HLib

import           Text.Regex.TDFA

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
    valuePtr <- peek nextPtr
    if valuePtr == nullPtr
        then return (key, Nothing)
        else peekCString (castPtr valuePtr)
            >>= \value -> return (key, Just value)
  where
    step = sizeOf ptr

    nextPtr = plusPtr ptr step

nextKeyValue :: Ptr (Ptr ()) -> Ptr (Ptr ())
nextKeyValue ptr = plusPtr (plusPtr ptr step) step
  where
    step = sizeOf ptr

parseValue :: String -> (String -> Maybe a) -> Map.Map String Value -> Maybe a
parseValue key parse metadatas = join (Map.lookup key metadatas) >>= parse

parseString :: String -> Maybe String
parseString value
    | match /= "" && t == "ASCII" && length raw + 1 == read components =
        Just raw
    | otherwise = Nothing
  where
    (raw, match, _, t : components : _) =
        value =~ " \\(.*, ([A-Za-z]+), ([0-9]+) components, [0-9]+ bytes\\)"
            :: (String, String, String, [String])

parseRationalList :: String -> Maybe [Rational]
parseRationalList value
    | match /= "" && t == "Rational" && length rationalsRaw == read components =
        Just rationals
    | otherwise = Nothing
  where
    rationalsRaw = getAllTextMatches (value =~ "[0-9]+/[0-9]+") :: [String]

    rationals = map ((\(n, _ : d) -> read n % read d) . break ('/' ==))
                    rationalsRaw

    (match : t : components : _) =
        getAllTextSubmatches (value =~ "([A-Za-z]+), ([0-9]+) components, [0-9]+ bytes\\)")
            :: [String]

parseRational :: String -> Maybe Rational
parseRational = parseRationalList >=> headOrNothing

parseDoubleList :: String -> Maybe [Double]
parseDoubleList = parseRationalList >=> return . map fromRational

parseDouble :: String -> Maybe Double
parseDouble = parseRational >=> return . fromRational

parseIntList :: String -> Maybe [Int]
parseIntList value
    | match /= "" && t == "Short" && length integersRaw == read components =
        Just integers
    | otherwise = Nothing
  where
    integers =
        map read (getAllTextMatches (integersRaw =~ "[0-9]+") :: [String])

    (integersRaw, match, _, t : components : _) =
        (value =~ " \\(.*, ([A-Za-z]+), ([0-9]+) components, [0-9]+ bytes\\)")
            :: (String, String, String, [String])

parseInt :: String -> Maybe Int
parseInt = parseIntList >=> headOrNothing

