{-# LANGUAGE OverloadedStrings #-}

module Read
    ( getApiMedia
    , getApiMediaById
    , MediaQuery(..)
    , getMedia
    , getMediaById
    , defaultMediaQuery
    ) where

import           Control.Monad.IO.Class

import           Data.Int
import qualified Data.List              as L
import           Data.Maybe
import           Data.Text.Lazy

import           Database.EJDB2         as EJDB2

import qualified Media                  as M

import           Repository

import           Web.Scotty

data MediaQuery = MediaQuery { tags :: [Text], offset :: Int, limit :: Int }

getApiMedia :: Repository -> ScottyM ()
getApiMedia repository = get "/api/media/" $ do
    query <- mediaQueryFromParams <$> params
    media <- liftIO $ getMedia repository query
    json media

mediaQueryFromParams :: [Param] -> MediaQuery
mediaQueryFromParams params =
    MediaQuery { tags   = maybe [] (splitOn ",") (lookup "tags" params)
               , offset = maybe 0 (read . unpack) (lookup "offset" params)
               , limit  = maybe 25 (read . unpack) (lookup "limit" params)
               }

tagsFilterQuery :: [Text] -> String
tagsFilterQuery [] = ""
tagsFilterQuery [ tag ] = "\"" ++ unpack (backSlash tag) ++ "\""
tagsFilterQuery (tag : tags) = tagsFilterQuery [ tag ] ++ tagsFilterQuery tags

backSlash :: Text -> Text
backSlash = replace "\"" "\\\""

mediaQueryToQuery :: MediaQuery -> Query ()
mediaQueryToQuery mediaQuery
    | (not . L.null) (tags mediaQuery) =
        Query ("@media/tags/[** in [" ++ tagsFilterQuery (tags mediaQuery)
               ++ "]] | limit :limit skip :offset " ++ sortQuery) $ do
            liftIO (putStrLn ("@media/tags/[** in ["
                              ++ tagsFilterQuery (tags mediaQuery)
                              ++ "]] | limit :limit skip :offset " ++ sortQuery))
            setI64 (fromIntegral $ limit mediaQuery) "limit"
            setI64 (fromIntegral $ offset mediaQuery) "offset"
    | otherwise = Query ("@media/[* not ni \"tags\"] or /[tags not ni \"trash\"] | limit :limit skip :offset "
                         ++ sortQuery) $ do
        setI64 (fromIntegral $ limit mediaQuery) "limit"
        setI64 (fromIntegral $ offset mediaQuery) "offset"

sortQuery :: String
sortQuery = "desc /date desc /importDate"

getApiMediaById :: Repository -> ScottyM ()
getApiMediaById (Repository _ database) =
    get "/api/media/:id"
        (param "id" >>= (liftIO . getMediaById database) . fromIntegral . read
         >>= json)

defaultMediaQuery :: MediaQuery
defaultMediaQuery = MediaQuery { tags = [], offset = 0, limit = 500 }

getMedia :: Repository -> MediaQuery -> IO [M.Media]
getMedia (Repository _ database) mediaQuery =
    getCount database (mediaQueryToQuery mediaQuery) >>= print >> catMaybes
    <$> getList' database (mediaQueryToQuery mediaQuery)

setId :: (Int64, Maybe M.Media) -> Maybe M.Media
setId (_, Nothing) = Nothing
setId (id, Just media) = Just (media { M.id = Just id })

getMediaById :: Database -> Int64 -> IO M.Media
getMediaById database id = getById database mediaCollection id
    >>= maybe (fail "media not found")
              (\media -> return $ media { M.id = Just id })

