{-# LANGUAGE OverloadedStrings #-}

module TgMonad where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Network.HTTP.Client
import Network.HTTP.Simple
import Types
import Utils

data TgBot m a = TgBot { runTgBot :: Config -> m a }

instance (Monad m) => Functor (TgBot m) where
  fmap f a = TgBot $ \c -> do
    t <- runTgBot a c
    return $ f t

instance (Monad m) => Applicative (TgBot m) where
  pure a = TgBot $ \_ -> return a
  f <*> a = TgBot $ \c -> do
    f' <- runTgBot f c
    a' <- runTgBot a c
    return $ f' a'

instance (Monad m) => Monad (TgBot m) where
  return = pure
  m >>= f = TgBot $ \c -> do
    a <- runTgBot m c
    runTgBot (f a) c

instance (MonadIO m) => MonadIO (TgBot m) where
  liftIO action = TgBot $ \_ -> liftIO action

getConfig :: (Monad m) => TgBot m Config
getConfig = TgBot $ \c -> return c

lift :: (Monad m) => m a -> TgBot m a
lift m = TgBot $ \_ -> do
  a <- m
  return a

-- `catch` lifted to TgBot
catch' :: TgBot IO a -> (SomeException -> TgBot IO a) -> TgBot IO a
catch' action handler = do
  config <- getConfig
  let h e = runTgBot (handler e) config
  liftIO $ catch (runTgBot action config) h

getUpdates :: Int -> TgBot IO (TgResponse [TgUpdate])
getUpdates offset = do
    config <- getConfig
    let url = apiGet (token config) "getUpdates" [("offset", Just (offset)), ("timeout", Just (timeout))]
    fmap getResponseBody $ httpJSON url
  where
    timeout = 300

sendMessage :: Int -> String -> TgBot IO (TgResponse TgMessage)
sendMessage target msg = do
  config <- getConfig
  let url = apiPost (token config) "sendMessage" [("chat_id", show target), ("text", msg)]
  fmap getResponseBody $ httpJSON url