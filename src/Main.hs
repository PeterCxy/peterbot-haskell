{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async
import Control.Exception
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe
import System.IO
import Data.ByteString.Conversion
import qualified Data.ByteString.Lazy.Char8 as BS
import Network.HTTP.Simple
import Types
import Utils

main :: IO ()
main = do
    configStr <- catch (openFile confFile ReadMode >>= hGetContents) openFail
    let configM = decode $ BS.pack configStr :: Maybe Config

    case configM of
      Nothing -> putStrLn "Failed to load config"
      Just config -> do
        (async $ fetchUpdates config $ -1) >>= wait
        return ()
  where
    confFile = "config.json"
    openFail :: IOError -> IO (String)
    openFail ex = do
      putStrLn $ "Failed to open file " ++ confFile
      print ex
      throwIO ex

fetchUpdates :: Config -> Int -> IO ()
fetchUpdates config index = do
    res <- fmap getResponseBody $ httpJSON url :: IO (TgResponse [TgUpdate])
    putStrLn $ if ok res then "Haha" else "Oops"
  where
    url = apiGet (token config) "getUpdates" [("offset", Just (index + 1))]
