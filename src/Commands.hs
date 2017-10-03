{-# LANGUAGE OverloadedStrings #-}

module Commands where

import Control.Concurrent.Async
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
    subscriber :: (String, Command) -> EventBus TgUpdate -> TgUpdate -> Maybe (IO ())
    subscriber pair bus ev = do
      msg <- message ev
      txt <- text msg
      let args = T.splitOn " " $ T.strip txt
      if (length args /= 0) && (head args == T.concat ["/", T.pack (fst pair)])
        then return $ snd pair config bus msg args
        else return $ return () -- Remeber this is Maybe (IO ()), so we need two `return`s

    reg :: (String, Command) -> IO ()
    reg pair = do
      _ <- subscribe bus $ \_ b ev -> do
        -- Defer the work to another function wrapped with Maybe monad
        -- Otherwise we will have to use bunches of case-of
        let m = subscriber pair b ev
        defVal m $ return () -- Force unwrap the Maybe monad for evaluation
      return ()

cmdHello :: Command
cmdHello config bus msg args = do
  _ <- async $ sendMessage config (Types.id $ chat msg) "Hello!"
  return ()