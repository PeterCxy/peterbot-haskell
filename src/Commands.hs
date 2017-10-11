{-# LANGUAGE OverloadedStrings #-}

module Commands where

import Control.Concurrent.Async
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import qualified Data.Text as T
import Data.UUID
import Data.List as L (intercalate)
import Text.Printf
import Text.Read (readMaybe)
import Types
import EventBus
import Utils
import TgMonad
import Calc

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
      ("chat_id", cmdChatId),
      ("print", cmdPrint),
      ("rpn", cmdRPN),
      ("calc", cmdCalc),
      ("eval1", cmdEval1),
      ("solve", cmdSolve),
      ("send", cmdSend)]

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
  _ <- replyMessage msg $ "Invalid argument for " ++ T.unpack cmd
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
\To invoke a command: /cmd_name[@%s] arg1 arg2 \"some argument with spaces\" ....\n\
\[] means that the part inside it is optional\n\
\Available commands:\n\
\    /hello - say Hello\n\
\    /info - print this information\n\
\    /my_id - get your Telegram ID (internal ID)\n\
\    /chat_id - get the internal ID of the current chat / group / channel\n\
\    /print - print the arguments as-is\n\
\    /rpn - Convert an infix expression to Reverse-Polish Notation (RPN)\n\
\    /calc - A simple calculator (Please use (0-x) instead of (-x) as the negative sign for now)\n\
\" (admin config) (bot_name config)
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

cmdPrint :: Command
cmdPrint bus msg ["print"] = invalidArgument bus msg ["print"]
cmdPrint _ msg args = do
  _ <- sendMessage (Types.chat_id $ chat msg) $ (T.unpack . T.unlines . tail) args
  return ()

cmdRPN :: Command
cmdRPN bus msg ["rpn"] = invalidArgument bus msg ["rpn"]
cmdRPN _ msg args = do
  let res = infix2RPN $ (T.unpack . T.unwords . tail) args
  case res of
    Nothing -> do
      _ <- replyMessage msg "Error parsing infix expression (maybe mismatched parentheses?)"
      return ()
    Just r -> do
      _ <- replyMessage msg $ L.intercalate " " r
      return ()

cmdCalc :: Command
cmdCalc bus msg ["calc"] = invalidArgument bus msg ["calc"]
cmdCalc _ msg args =
  processCalcResult msg $ calc $ (T.unpack . T.unwords . tail) args

cmdEval1 :: Command
cmdEval1 bus msg ["eval1"] = invalidArgument bus msg ["eval1"]
cmdEval1 bus msg ("eval1":x:[]) = invalidArgument bus msg ["eval1"]
cmdEval1 _ msg ("eval1":x:str) =
  processCalcResult msg $ eval1_ ((T.unpack . T.unwords) str) $ T.unpack x

cmdSolve :: Command
cmdSolve bus msg ["solve"] = invalidArgument bus msg ["solve"]
cmdSolve bus msg ("solve":x:[]) = invalidArgument bus msg ["solve"]
cmdSolve _ msg ("solve":x:str) =
  processCalcResult msg $ solveNewton_ (eval1 ((T.unpack . T.unwords) str)) 30 $ T.unpack x

-- Helper function to process the result of calculations
processCalcResult :: TgMessage -> Either String Double -> TgBot IO ()
processCalcResult msg res = case res of
  Left err -> do
    _ <- replyMessage msg ("Error: " ++ err)
    return ()
  Right r -> do
    _ <- replyMessage msg ("Result: " ++ (show r))
    return ()

-- Secret: send to chat
-- The admin can send a message to some chat, or echo to the sender if the chat ID is illegal
-- /send chat_id text.... (text can contain spaces; will be rejoined in this function)
cmdSend :: Command
cmdSend _ msg ("send" : chatId : txt) = do
  userIsAdmin <- liftIdentity $ isAdmin $ from_user msg
  if userIsAdmin
    then do
      _ <- sendMessage (defVal (readMaybe $ T.unpack chatId) $ Types.chat_id $ chat msg) $ T.unpack $ T.unwords txt
      return ()
    else return ()
cmdSend _ _ _ = return () -- Be silent. Don't tell them anything