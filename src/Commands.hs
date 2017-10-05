{-# LANGUAGE OverloadedStrings #-}

module Commands where

import Control.Concurrent.Async
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import qualified Data.Text as T
import Data.UUID
import Types
import EventBus
import Utils

type Command = Config -> EventBus TgUpdate -> TgMessage -> [T.Text] -> IO ()

-- Register commands
-- Command messages should be in the following format:
--   /cmd_name arg1 arg2 arg3 ....
-- TODO: Make a proper parser for the cmdline
registerCommands :: Config -> EventBus TgUpdate -> IO ()
registerCommands config bus = do
    _ <- mapM reg cmds
    return ()
  where
    -- List of commands and their corresponding functions
    cmds = [
      ("hello", cmdHello)]
    subscriber :: (String, Command) -> EventBus TgUpdate -> TgUpdate -> MaybeT IO ()
    subscriber pair bus ev = do
      msg <- MaybeT $ return $ message ev -- Lift Maybe into MaybeT
      txt <- MaybeT $ return $ text msg
      let args = parseArgs txt
      if (length args /= 0) && (head args == T.concat ["/", T.pack (fst pair)])
        then lift $ snd pair config bus msg args
        else lift $ return () -- Pretend that we have done some IO work

    reg :: (String, Command) -> IO ()
    reg pair = do
      _ <- subscribe bus $ \_ b ev -> do
        -- Defer the work to another function wrapped with MaybeT
        -- Otherwise we will have to use bunches of case-of
        m <- runMaybeT $ subscriber pair b ev -- IO (Maybe ())
        return $ defVal m $ () -- Force unwrap the inner Maybe monad
      return ()

cmdHello :: Command
cmdHello config _ msg ["/hello"] = do
  _ <- async $ sendMessage config (Types.id $ chat msg) "Hello!"
  return ()
cmdHello config _ msg _ = do
  _ <- async $ sendMessage config (Types.id $ chat msg) "Hello but I do not take any argument, sorry."
  return ()