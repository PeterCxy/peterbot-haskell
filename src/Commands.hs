{-# LANGUAGE OverloadedStrings #-}

module Commands where

import Control.Concurrent.Async
import Control.Monad.State.Lazy
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import qualified Data.Text as T
import Data.UUID
import Types
import EventBus
import Utils

type Command = EventBus TgUpdate -> TgMessage -> [T.Text] -> TgBot ()

-- Register commands
-- Command messages should be in the following format:
--   /cmd_name arg1 arg2 arg3 ....
-- TODO: Make a proper parser for the cmdline
registerCommands :: EventBus TgUpdate -> TgBot ()
registerCommands bus = do
    _ <- mapM reg cmds
    return ()
  where
    -- List of commands and their corresponding functions
    cmds = [
      ("hello", cmdHello)]
    subscriber :: (String, Command) -> EventBus TgUpdate -> TgUpdate -> MaybeT (StateT Config IO) ()
    subscriber pair bus ev = do
      msg <- MaybeT $ return $ message ev -- Lift Maybe into MaybeT
      txt <- MaybeT $ return $ text msg
      let args = parseArgs txt
      if (length args /= 0) && (head args == T.concat ["/", T.pack (fst pair)])
        then do
          config <- get
          _ <- liftIO $ async $ runStateT (snd pair bus msg args) config -- Default to Async
          return ()
        else liftIO $ return () -- Pretend that we have done some IO work

    reg :: (String, Command) -> TgBot ()
    reg pair = do
      config <- get
      _ <- liftIO $ subscribe bus $ \_ b ev -> do
        -- Defer the work to another function wrapped with MaybeT
        -- Otherwise we will have to use bunches of case-of
        m <- runStateT (runMaybeT $ subscriber pair b ev) config -- IO (Maybe ((), Config))
        return $ defVal (fst m) $ () -- Force unwrap the inner Maybe monad
      return ()

cmdHello :: Command
cmdHello _ msg ["/hello"] = do
  _ <- sendMessage (Types.id $ chat msg) "Hello!"
  return ()
cmdHello _ msg _ = do
  _ <- sendMessage (Types.id $ chat msg) "Hello but I do not take any argument, sorry."
  return ()