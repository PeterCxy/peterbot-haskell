{-# LANGUAGE OverloadedStrings #-}

module Commands where

import Control.Concurrent.Async
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import qualified Data.Text as T
import Data.UUID
import Types
import EventBus
import Utils
import TgMonad

type Command = EventBus TgUpdate -> TgMessage -> [T.Text] -> TgBot IO ()

isCommand :: String -> [T.Text] -> Maybe [T.Text]
isCommand cmd args =
  if (length args /= 0) && (head args == T.concat ["/", T.pack cmd]) -- TODO: support commands like /cmd@bot_name
    then Just args
    else Nothing

-- Register commands
-- Command messages should be in the following format:
--   /cmd_name arg1 arg2 arg3 ....
-- TODO: Make a proper parser for the cmdline
registerCommands :: EventBus TgUpdate -> TgBot IO ()
registerCommands bus = do
    _ <- mapM reg cmds
    return ()
  where
    -- List of commands and their corresponding functions
    cmds = [
      ("hello", cmdHello),
      ("myId", cmdMyId),
      ("chatId", cmdChatId)]
    subscriber :: (String, Command) -> EventBus TgUpdate -> TgUpdate -> MaybeT (TgBot IO) ()
    subscriber pair bus ev = do
      msg <- liftMaybe $ message ev -- Lift Maybe into MaybeT
      txt <- liftMaybe $ text msg
      config <- lift getConfig
      args <- liftMaybe $ isCommand (fst pair) $ parseArgs txt -- TODO: Don't even parse for non-command messages
      _ <- liftIO $ async $ runTgBot (snd pair bus msg args) config -- Spawn a new thread by default and pass the telegram arguments
      return ()

    reg :: (String, Command) -> TgBot IO ()
    reg pair = do
      config <- getConfig -- Save the config since we will use it in another lambda
      _ <- liftIO $ subscribe bus $ \_ b ev -> do
        -- Defer the work to another function wrapped with MaybeT
        -- Otherwise we will have to use bunches of case-of
        m <- runTgBot (runMaybeT $ subscriber pair b ev) config -- IO (Maybe ())
        return $ defVal m $ () -- Force unwrap the inner Maybe monad
      return ()

invalidArgument :: Command
invalidArgument _ msg (cmd:_) = do
  _ <- sendMessage (Types.chat_id $ chat msg) $ "Invalid argument for " ++ T.unpack cmd
  return ()

cmdHello :: Command
cmdHello _ msg ["/hello"] = do
  _ <- sendMessage (Types.chat_id $ chat msg) "Hello!"
  return ()
cmdHello bus msg list = invalidArgument bus msg list

cmdMyId :: Command
cmdMyId _ msg ["/myId"] = do
    _ <- sendMessage (Types.chat_id $ chat msg) $ "Your ID: " ++ idStr
    return ()
  where
    idStr = show $ defVal (fmap user_id $ from_user msg) $ -1
cmdMyId bus msg list = invalidArgument bus msg list

cmdChatId :: Command
cmdChatId _ msg ["/chatId"] = do
    _ <- sendMessage (Types.chat_id $ chat msg) $ "Chat ID: " ++ idStr
    return ()
  where
    idStr = show $ chat_id $ chat msg
cmdChatId bus msg list = invalidArgument bus msg list