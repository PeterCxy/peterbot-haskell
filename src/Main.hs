{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent.Async
import Control.Monad
import Control.Monad.IO.Class
import Control.Exception
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe
import System.IO
import Data.ByteString.Conversion
import qualified Data.ByteString.Lazy.Char8 as BS
import Types
import Utils
import EventBus
import Commands
import TgMonad

main :: IO ()
main = do
    configStr <- catch (openFile confFile ReadMode >>= hGetContents) openFail
    let config = assertM "Failed to load config" $ decode $ BS.pack configStr :: Config

    bus <- createBus >>= runBus :: IO (EventBus TgUpdate)
    runTgBot (registerSubscribers bus) config
    --runBus bus
    (async $ runTgBot (fetchUpdates bus $ -1) config) >>= wait
  where
    confFile = "config.json"
    openFail :: IOError -> IO (String)
    openFail ex = do
      putStrLn $ "Failed to open file " ++ confFile
      print ex
      throwIO ex

fetchUpdates :: EventBus TgUpdate -> Int -> TgBot IO ()
fetchUpdates bus index = do
    res <- catch' (getUpdates $ index + 1) fetchFail :: TgBot IO (TgResponse [TgUpdate])
    -- Default to [] for failed cases
    -- ifM: return Nothing if the response is not Okay
    let upd = defVal (result res >>= \r -> ifM r $ ok res) []
    liftIO $ mapM (publish bus) upd
    if length upd == 0
      then next index -- No result received, retry
      else next $ update_id $ last upd
  where
    next :: Int -> TgBot IO ()
    next newIndex = fetchUpdates bus newIndex
    fetchFail :: SomeException -> TgBot IO (TgResponse [TgUpdate])
    fetchFail ex = do
      liftIO $ putStrLn "No message received before timeout or there were network issues. Continuing."
      -- Return a pseudo value
      return $ TgResponse False Nothing

registerSubscribers :: EventBus TgUpdate -> TgBot IO ()
registerSubscribers bus = do
  liftIO $ subscribe bus $ \_ _ ev -> do
    putStrLn "Received Message"
    print $ update_id ev
    --subscribe bus $ \_ _ _ -> print "My new subscriber!"
  registerCommands bus