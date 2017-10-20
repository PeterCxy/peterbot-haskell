{-# LANGUAGE OverloadedStrings #-}

module TgMonad
  ( TgBot (..)
  , getConfig
  , liftIdentity
  , catch'
  , getUpdates
  , sendMessage
  , replyMessage
  , isAdmin
  , isAdmin'
  , isBlacklisted
  , isBlacklisted'
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Data.ByteString
import Data.ByteString.Conversion
import qualified Data.Text as T
import Network.HTTP.Client
import Network.HTTP.Simple
import Types
import Utils

data TgBot m a = TgBot { runTgBot :: Config -> m a }
type TgBotI a = TgBot Identity a

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

instance MonadTrans TgBot where
  lift m = TgBot $ \_ -> do
    a <- m
    return a

getConfig :: (Monad m) => TgBot m Config
getConfig = TgBot $ \c -> return c

hoist :: (Monad m) => (m a -> n a) -> TgBot m a -> TgBot n a
hoist f a = TgBot $ \c -> f $ runTgBot a c

liftIdentity :: (Monad m) => TgBotI a -> TgBot m a
liftIdentity = hoist generalize

-- `catch` lifted to TgBot
catch' :: TgBot IO a -> (SomeException -> TgBot IO a) -> TgBot IO a
catch' action handler = do
  config <- getConfig
  let h e = runTgBot (handler e) config
  liftIO $ catch (runTgBot action config) h

apiURL :: T.Text -> TgBotI T.Text
apiURL api = do
    config <- getConfig
    return $ T.concat [baseURL, token config, "/", api]
  where baseURL = "https://api.telegram.org/bot"

-- Build a GET Request for Telegram API
apiGet :: (ToByteString a) => T.Text -> [(ByteString, Maybe a)] -> TgBotI Request
apiGet api qs = do
  url <- apiURL api
  let req = parseRequest_ $ "GET " ++ T.unpack url
  return $ setRequestQueryString (toQS qs) req

-- Build a POST Request for Telegram API
apiPost :: (ToByteString a) => T.Text -> [(ByteString, a)] -> TgBotI Request
apiPost api opt = do
  url <- apiURL api
  let req = parseRequest_ $ "POST " ++ T.unpack url
  return $ urlEncodedBody (toBody opt) req

getUpdates :: Int -> TgBot IO (TgResponse [TgUpdate])
getUpdates offset = do
    url <- liftIdentity $ apiGet "getUpdates" [("offset", Just (offset)), ("timeout", Just (timeout))]
    fmap getResponseBody $ httpJSON url
  where
    timeout = 30

sendMessage :: Int -> String -> TgBot IO (TgResponse TgMessage)
sendMessage target msg = do
  url <- liftIdentity $ apiPost "sendMessage" [("chat_id", show target), ("text", msg)]
  fmap getResponseBody $ httpJSON url

replyMessage :: TgMessage -> String -> TgBot IO (TgResponse TgMessage)
replyMessage original msg = do
    url <- liftIdentity $ apiPost "sendMessage" [
      ("chat_id", show $ chat_id $ chat original),
      ("text", msg),
      ("reply_to_message_id", show $ message_id original)]
    fmap getResponseBody $ httpJSON url

isAdmin :: Maybe TgUser -> TgBotI Bool
isAdmin u = do
  res <- liftIdentity $ runMaybeT $ isAdmin' u
  return $ defVal res False

isAdmin' :: Maybe TgUser -> MaybeT (TgBot Identity) Bool
isAdmin' u = do
  user <- liftMaybe $ (u >>= user_name)
  config <- lift $ getConfig
  return $ user == T.unpack (admin config)

isBlacklisted :: Maybe TgUser -> TgBotI Bool
isBlacklisted u = do
  res <- liftIdentity $ runMaybeT $ isBlacklisted' u
  return $ defVal res False

isBlacklisted' :: Maybe TgUser -> MaybeT (TgBot Identity) Bool
isBlacklisted' u = do
  uid <- liftMaybe $ (fmap user_id u)
  config <- lift $ getConfig
  bl <- liftMaybe $ blacklist config
  return $ uid `Prelude.elem` bl