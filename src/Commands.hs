{-# LANGUAGE OverloadedStrings #-}

module Commands where

import Control.Concurrent.Async
import Control.Monad.IO.Class
import Control.Monad.Trans.Class as TC
import Control.Monad.Trans.Maybe
import qualified Data.Text as T
import Data.UUID
import Types
import EventBus
import Utils
import TgMonad

type Command = EventBus TgUpdate -> TgMessage -> [T.Text] -> TgBot IO ()

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
      ("hello", cmdHello)]
    subscriber :: (String, Command) -> EventBus TgUpdate -> TgUpdate -> MaybeT (TgBot IO) ()
    subscriber pair bus ev = do
      msg <- MaybeT $ return $ message ev -- Lift Maybe into MaybeT
      txt <- MaybeT $ return $ text msg
      config <- TC.lift getConfig
      let args = parseArgs txt
      if (length args /= 0) && (head args == T.concat ["/", T.pack (fst pair)])
        then do
          _ <- liftIO $ async $ runTgBot (snd pair bus msg args) config -- Spawn a new thread by default and pass the telegram arguments
          return ()
        else liftIO $ return () -- Pretend that we have done some IO work

    reg :: (String, Command) -> TgBot IO ()
    reg pair = do
      config <- getConfig -- Save the config since we will use it in another lambda
      _ <- liftIO $ subscribe bus $ \_ b ev -> do
        -- Defer the work to another function wrapped with MaybeT
        -- Otherwise we will have to use bunches of case-of
        m <- runTgBot (runMaybeT $ subscriber pair b ev) config -- IO (Maybe ())
        return $ defVal m $ () -- Force unwrap the inner Maybe monad
      return ()

cmdHello :: Command
cmdHello _ msg ["/hello"] = do
  _ <- sendMessage (Types.id $ chat msg) "Hello!"
  return ()
cmdHello _ msg _ = do
  _ <- sendMessage (Types.id $ chat msg) "Hello but I do not take any argument, sorry."
  return ()