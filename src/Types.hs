{-# LANGUAGE OverloadedStrings, ExistentialQuantification #-}

module Types where

import Data.Aeson
import qualified Data.Text as T

data Config = Config {
  token :: T.Text,
  admin :: T.Text,
  bot_name :: T.Text
}

instance FromJSON Config where
  parseJSON = withObject "Config" $ \c -> Config
    <$> c .: "token"
    <*> c .: "admin"
    <*> c .: "bot_name"

class TgResult t where
  tid :: t -> Int

instance (TgResult t) => TgResult [t] where
  tid [] = -1
  tid t = tid $ head t

data TgResponse t = (TgResult t) => TgResponse {
  ok :: Bool,
  result :: Maybe (t)
}

instance (FromJSON t, TgResult t) => FromJSON (TgResponse t) where
  parseJSON = withObject "TgResponse" $ \r -> TgResponse
   <$> r .: "ok"
   <*> r .:? "result"

data TgUpdate = TgUpdate {
  update_id :: Int,
  message :: Maybe TgMessage
}

instance TgResult TgUpdate where
  tid t = update_id t

instance FromJSON TgUpdate where
  parseJSON = withObject "TgUpdate" $ \u -> TgUpdate
   <$> u .: "update_id"
   <*> u .:? "message"

data TgMessage = TgMessage {
  message_id :: Int,
  chat :: TgChat,
  text :: Maybe T.Text,
  from_user :: Maybe TgUser
}

instance TgResult TgMessage where
  tid t = message_id t

instance FromJSON TgMessage where
  parseJSON = withObject "TgMessage" $ \m -> TgMessage
    <$> m .: "message_id"
    <*> m .: "chat"
    <*> m .:? "text"
    <*> m .:? "from"

data TgChat = TgChat {
  chat_id :: Int
}

instance FromJSON TgChat where
  parseJSON = withObject "TgChat" $ \c -> TgChat
    <$> c .: "id"

data TgUser = TgUser {
  user_id :: Int,
  user_name :: Maybe String
}

instance FromJSON TgUser where
  parseJSON = withObject "TgUser" $ \u -> TgUser
   <$> u .: "id"
   <*> u .:? "username"