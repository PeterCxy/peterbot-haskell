{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async
import Control.Monad
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
    res <- catch (fmap getResponseBody $ httpJSON url) fetchFail :: IO (TgResponse [TgUpdate])
    case ok res of
      False -> next index -- Not okay (maybe exception), retry this request
      True -> case result res of
        Nothing -> next index -- Nothing returned as Result, retry
        Just upd -> do
          print $ length upd
          if length upd == 0
            then next index -- No result received, retry
            else next $ update_id $ last upd
  where
    url = apiGet (token config) "getUpdates" [("offset", Just (index + 1))]
    next :: Int -> IO ()
    next newIndex = fetchUpdates config newIndex
    fetchFail :: SomeException -> IO (TgResponse [TgUpdate])
    fetchFail ex = do
      putStrLn "Failed to fetch updates from Telegram. Skipping."
      -- Return a pseudo value
      return $ TgResponse False Nothing
