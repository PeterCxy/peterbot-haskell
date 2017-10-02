{-# LANGUAGE OverloadedStrings #-}

module Utils where

import Data.ByteString
import Data.ByteString.Conversion
import qualified Data.Text as T
import Network.HTTP.Simple
import System.IO

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

-- Convert from (ByteString, something) to (ByteString, Maybe ByteString) which is required by HTTP's QueryString
toQS :: (ToByteString a) => [(ByteString, Maybe a)] -> [(ByteString, Maybe ByteString)]
toQS = Prelude.map (\tup -> (fst tup, fmap toByteString' $ snd tup))
