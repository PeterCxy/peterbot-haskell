{-# LANGUAGE OverloadedStrings #-}

module Utils where

import Data.ByteString
import Data.ByteString.Conversion
import qualified Data.Text as T
import Network.HTTP.Client
import Network.HTTP.Simple
import System.IO
import Types

apiURL :: T.Text -> T.Text -> T.Text
apiURL token api = T.concat [baseURL, token, "/", api]
  where baseURL = "https://api.telegram.org/bot"

-- Build a GET Request for Telegram API
apiGet :: (ToByteString a) => T.Text -> T.Text -> [(ByteString, Maybe a)] -> Request
apiGet token api qs =
    setRequestQueryString (toQS qs) req
  where
    url = apiURL token api
    req = parseRequest_ $ "GET " ++ T.unpack url

-- Build a POST Request for Telegram API
apiPost :: (ToByteString a) => T.Text -> T.Text -> [(ByteString, a)] -> Request
apiPost token api opt =
    urlEncodedBody (toBody opt) req
  where
    url = apiURL token api
    req = parseRequest_ $ "POST " ++ T.unpack url

-- Convert from (ByteString, something) to (ByteString, Maybe ByteString) which is required by HTTP's QueryString
toQS :: (ToByteString a) => [(ByteString, Maybe a)] -> [(ByteString, Maybe ByteString)]
toQS = Prelude.map (\tup -> (fst tup, fmap toByteString' $ snd tup))

-- Convert from (ByteString, something) to (ByteString, ByteString) which is required by HTTP's urlencoded body
toBody :: (ToByteString a) => [(ByteString, a)] -> [(ByteString, ByteString)]
toBody = Prelude.map (\tup -> (fst tup, toByteString' $ snd tup))

getUpdates :: Config -> Int -> IO (TgResponse [TgUpdate])
getUpdates config offset =
    fmap getResponseBody $ httpJSON url
  where
    url = apiGet (token config) "getUpdates" [("offset", Just (offset))]

sendMessage :: Config -> Int -> String -> IO (TgResponse TgMessage)
sendMessage config target msg =
    fmap getResponseBody $ httpJSON url
  where
    url = apiPost (token config) "sendMessage" [("chat_id", show target), ("text", msg)]