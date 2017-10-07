{-# LANGUAGE OverloadedStrings #-}

module Commands where

import Control.Concurrent.Async
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import qualified Data.Text as T
import Data.UUID
import Text.Printf
import Types
import EventBus
import Utils
import TgMonad

type Command = EventBus TgUpdate -> TgMessage -> [T.Text] -> TgBot IO ()

couldBeCommand :: T.Text -> Maybe T.Text
couldBeCommand str = ifM str $ (T.head str) == '/'

isCommand :: String -> T.Text -> [T.Text] -> Maybe [T.Text]
isCommand cmd botName args = ifM ((T.pack cmd) : tail args) $ (length args /= 0)
  && (head args == T.concat ["/", T.pack cmd] || head args == T.concat ["/", T.pack cmd, "@", botName])

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
      ("info", cmdInfo),
      ("my_id", cmdMyId),
      ("chat_id", cmdChatId)]

    subscriber :: (String, Command) -> EventBus TgUpdate -> TgUpdate -> MaybeT (TgBot IO) ()
    subscriber pair bus ev = do
      msg <- liftMaybe $ message ev -- Lift Maybe into MaybeT
      txt <- liftMaybe $ (text msg >>= couldBeCommand)
      config <- lift getConfig
      args <- liftMaybe $ isCommand (fst pair) (bot_name config) $ parseArgs txt -- TODO: Don't even parse for non-command messages
      _ <- liftIO $ async $ runTgBot (snd pair bus msg $ args) config -- Spawn a new thread by default and pass the telegram arguments
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
invalidArgument _ _ _ = fail "What the heck?"

cmdHello :: Command
cmdHello _ msg ["hello"] = do
  _ <- sendMessage (Types.chat_id $ chat msg) "Hello!"
  return ()
cmdHello bus msg list = invalidArgument bus msg list

cmdInfo :: Command
cmdInfo _ msg ["info"] = do
  config <- getConfig
  let info = printf "\
\Hello, this is the bot of @%s, written in Haskell.\n\
\Source code available at https://github.com/PeterCxy/peterbot-haskell\n\
\Available commands:\n\
\    /hello - say Hello\n\
\    /info - print this information\n\
\    /my_id - get your Telegram ID (internal ID)\n\
\    /chat_id - get the internal ID of the current chat / group / channel\
\" (admin config)
  _ <- replyMessage msg info
  return ()
cmdInfo bus msg list = invalidArgument bus msg list

cmdMyId :: Command
cmdMyId _ msg ["my_id"] = do
    _ <- replyMessage msg $ "Your ID: " ++ idStr
    return ()
  where
    idStr = show $ defVal (fmap user_id $ from_user msg) $ -1
cmdMyId bus msg list = invalidArgument bus msg list

cmdChatId :: Command
cmdChatId _ msg ["chat_id"] = do
    _ <- sendMessage (Types.chat_id $ chat msg) $ "Chat ID: " ++ idStr
    return ()
  where
    idStr = show $ chat_id $ chat msg
cmdChatId bus msg list = invalidArgument bus msg list