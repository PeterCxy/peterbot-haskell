{-# LANGUAGE OverloadedStrings #-}

module Commands where

import Control.Concurrent.Async
import qualified Data.Text as T
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
    mapM reg cmds
    return ()
  where
    -- List of commands and their corresponding functions
    cmds = [
      ("hello", cmdHello)]
    reg :: (String, Command) -> IO ()
    reg pair = do
      subscribe bus $ \uid b ev ->
        case message ev of
          Nothing -> return ()
          Just msg -> case text msg of
            Nothing -> return ()
            Just txt -> do
              let args = T.splitOn " " $ T.strip txt
              --print args
              if (length args /= 0) && (head args == T.concat ["/", T.pack (fst pair)])
                then snd pair config b msg args
                else return ()
      return ()

cmdHello :: Command
cmdHello config bus msg args = do
  async $ sendMessage config (Types.id $ chat msg) "Hello!"
  return ()