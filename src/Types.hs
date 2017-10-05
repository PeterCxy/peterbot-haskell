{-# LANGUAGE OverloadedStrings, ExistentialQuantification #-}

module Types where

import Control.Monad.Trans.State
import Data.Aeson
import qualified Data.Text as T

data Config = Config {
  token :: T.Text,
  admin :: T.Text
}

type TgBot a = StateT Config IO a

instance FromJSON Config where
  parseJSON = withObject "Config" $ \c -> Config
    <$> c .: "token"
    <*> c .: "admin"

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
  text :: Maybe T.Text
}

instance TgResult TgMessage where
  tid t = message_id t

instance FromJSON TgMessage where
  parseJSON = withObject "TgMessage" $ \m -> TgMessage
    <$> m .: "message_id"
    <*> m .: "chat"
    <*> m .:? "text"

data TgChat = TgChat {
  id :: Int
}

instance FromJSON TgChat where
  parseJSON = withObject "TgChat" $ \c -> TgChat
    <$> c .: "id"