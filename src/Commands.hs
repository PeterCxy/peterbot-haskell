{-# LANGUAGE OverloadedStrings #-}

module Commands (registerCommands) where

import Control.Concurrent.Async
import Control.Monad (unless)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import qualified Data.Text as T
import Data.UUID
import Text.Printf
import Text.Read (readMaybe)
import Types
import EventBus
import Utils
import TgMonad
import Calc

type Command = EventBus TgUpdate -> TgMessage -> [T.Text] -> TgBot IO ()

couldBeCommand :: T.Text -> Maybe T.Text
couldBeCommand str = ifM str $ T.head str == '/'

isCommand :: String -> T.Text -> [T.Text] -> Maybe [T.Text]
isCommand cmd botName args = ifM (T.pack cmd : tail args) $ (length args /= 0)
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
      ("push", cmdPush),
      ("drop", cmdDrop),
      ("pop", cmdPop),
      ("dropAll", cmdDropAll),
      ("send", cmdSend)]

    subscriber :: (String, Command) -> EventBus TgUpdate -> TgUpdate -> MaybeT (TgBot IO) ()
    subscriber pair bus ev = do
      config <- lift getConfig
      msg <- liftMaybe $ message ev -- Lift Maybe into MaybeT
      blacklisted <- lift $ liftIdentity $ isBlacklisted $ from_user msg
      txt <- liftMaybe
        (text msg >>= couldBeCommand >>= \ m -> ifM m $ not blacklisted) -- Continue if the user is not blacklisted and it may be a command
      args <- liftMaybe $ isCommand (fst pair) (bot_name config) $ parseArgs txt
      _ <- liftIO $ async $ runTgBot (snd pair bus msg args) config -- Spawn a new thread by default and pass the telegram arguments
      return ()

    reg :: (String, Command) -> TgBot IO ()
    reg pair = do
      config <- getConfig -- Save the config since we will use it in another lambda
      _ <- liftIO $ subscribe bus $ \_ b ev -> do
        -- Defer the work to another function wrapped with MaybeT
        -- Otherwise we will have to use bunches of case-of
        m <- runTgBot (runMaybeT $ subscriber pair b ev) config -- IO (Maybe ())
        return $ defVal m () -- Force unwrap the inner Maybe monad
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
    let info = printf infoLines (admin config) (bot_name config)
    _ <- replyMessage msg info
    return ()
  where
    infoLines = unlines [
      "Hello, this is the bot of @%s, written in Haskell.",
      "Source code available at https://github.com/PeterCxy/peterbot-haskell",
      "To invoke a command: /cmd_name[@%s] arg1 arg2 \"some argument with spaces\" ....",
      "[] means that the part inside it is optional",
      "Available commands:",
      "    /hello - say Hello",
      "    /info - print this information",
      "    /my_id - get your Telegram ID (internal ID)",
      "    /chat_id - get the internal ID of the current chat / group / channel",
      "    /print - print the arguments as-is",
      "    /rpn - Convert an infix expression to Reverse-Polish Notation (RPN)",
      "    /calc - A simple calculator",
      "    /solve - <initial_value> <function> Solve f(x) = 0 by Newton's method where f = function"
      ]
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

cmdPush :: Command
cmdPush _ msg ["push", newItem] =
  cmdChangeTitle msg $ doCmdPush (chat_id $ chat msg) (chat_title $ chat msg) newItem
cmdPush bus msg _ = invalidArgument bus msg ["push"]

cmdDrop :: Command
cmdDrop _ msg ["drop"] =
  cmdChangeTitle msg $ doCmdDrop (chat_id $ chat msg) (chat_title $ chat msg)
cmdDrop bus msg _ = invalidArgument bus msg ["drop"]

cmdPop :: Command
cmdPop _ msg ["pop"] =
  cmdChangeTitle msg $ doCmdPop (chat_id $ chat msg) (chat_title $ chat msg)
cmdPop bus msg _ = invalidArgument bus msg ["pop"]

cmdDropAll :: Command
cmdDropAll _ msg ["dropAll"] =
  cmdChangeTitle msg $ doCmdDropAll (chat_id $ chat msg) (chat_title $ chat msg)
cmdDropAll bus msg _ = invalidArgument bus msg ["dropAll"]

-- Shared logic for title-changing actions
-- Do such actions only in groups
cmdChangeTitle :: TgMessage -> TgBot IO () -> TgBot IO ()
cmdChangeTitle msg ioAction
    | (chatType == "group") || (chatType == "supergroup") = ioAction
    | otherwise = do
      _ <- replyMessage msg "Can only do this within groups."
      return ()
  where
    chatType = chat_type $ chat msg

-- Implementation of title-changing actions
doCmdPush :: Int -> Maybe T.Text -> T.Text -> TgBot IO ()
doCmdPush _ Nothing _ = return ()
doCmdPush chatId (Just title) newItem = doChangeTitle chatId $ pushTitle title newItem

doCmdDrop :: Int -> Maybe T.Text -> TgBot IO ()
doCmdDrop _ Nothing = return ()
doCmdDrop chatId (Just title) = doChangeTitle chatId $ dropTitle title

doCmdPop :: Int -> Maybe T.Text -> TgBot IO ()
doCmdPop _ Nothing = return ()
doCmdPop chatId (Just title) = doChangeTitle chatId $ popTitle title

doCmdDropAll :: Int -> Maybe T.Text -> TgBot IO ()
doCmdDropAll _ Nothing = return ()
doCmdDropAll chatId (Just title) = doChangeTitle chatId $ dropAllTitle title

doChangeTitle :: Int -> T.Text -> TgBot IO ()
doChangeTitle chatId newTitle = do
    res <- setChatTitle chatId $ T.unpack newTitle
    case result res of
      Nothing -> failToChange
      Just b -> unless b failToChange
  where
    failToChange = do
      _ <- sendMessage chatId ("Could not change title to: \"" ++ T.unpack newTitle ++ "\"")
      return ()

-- Push newItem to title. Drop the last one if longer than 255 chars.
pushTitle :: T.Text -> T.Text -> T.Text
pushTitle title newItem
    | T.length newTitle >= 255 = dropTitle newTitle
    | otherwise = newTitle
  where
    newTitle = joinTitle $ newItem : splitTitle title

-- Drop the last item of the title.
dropTitle :: T.Text -> T.Text
dropTitle = joinTitle . init . splitTitle

-- Pop the first item out of the title
popTitle :: T.Text -> T.Text
popTitle = joinTitle . tail . splitTitle

-- Keep only the first item of the title
dropAllTitle :: T.Text -> T.Text
dropAllTitle = head . splitTitle

splitTitle :: T.Text -> [T.Text]
splitTitle = map T.strip . T.splitOn "||"

joinTitle :: [T.Text] -> T.Text
joinTitle = T.intercalate " || "

-- Calculator functions
cmdRPN :: Command
cmdRPN bus msg ["rpn"] = invalidArgument bus msg ["rpn"]
cmdRPN _ msg args = do
  let res = infix2RPN $ (T.unpack . T.unwords . tail) args
  case res of
    Nothing -> do
      _ <- replyMessage msg "Error parsing infix expression (maybe mismatched parentheses?)"
      return ()
    Just r -> do
      _ <- replyMessage msg $ unwords r
      return ()

cmdCalc :: Command
cmdCalc bus msg ["calc"] = invalidArgument bus msg ["calc"]
cmdCalc _ msg args =
  processCalcResult msg $ calc $ (T.unpack . T.unwords . tail) args

cmdEval1 :: Command
cmdEval1 bus msg ["eval1"] = invalidArgument bus msg ["eval1"]
cmdEval1 bus msg ["eval1", _] = invalidArgument bus msg ["eval1"]
cmdEval1 _ msg ("eval1":x:str) =
  processCalcResult msg $ eval1_ ((T.unpack . T.unwords) str) $ T.unpack x
cmdEval1 _ _ _ = return ()

cmdSolve :: Command
cmdSolve bus msg ["solve"] = invalidArgument bus msg ["solve"]
cmdSolve bus msg ["solve", _] = invalidArgument bus msg ["solve"]
cmdSolve _ msg ("solve":x:str) =
  processCalcResult msg $ solveNewton_ (eval1 ((T.unpack . T.unwords) str)) 30 $ T.unpack x
cmdSolve _ _ _ = return ()

-- Helper function to process the result of calculations
processCalcResult :: TgMessage -> Either String Double -> TgBot IO ()
processCalcResult msg res = case res of
  Left err -> do
    _ <- replyMessage msg ("Error: " ++ err)
    return ()
  Right r -> do
    _ <- replyMessage msg ("Result: " ++ show r)
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