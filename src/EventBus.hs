{-# LANGUAGE RecordWildCards #-}

module EventBus
  ( EventBus (..)
  , createBus
  , runBus
  , subscribe
  , subscribeOnce
  , unsubscribe
  , publish
  ) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Data.UUID
import Data.UUID.V4
import qualified Data.Vector as V
import System.IO
import Utils

-- A simple and naive EventBus implementation
-- With STM
-- Since things are immutable in Haskell, the implementation makes use of the mutable TVar
-- By which every time a event is received, the EventBus itself is replaced with a modified one
-- Thus we can add / remove subscribers
-- Note that subscribers (Handlers) are executed inside the EventBus thread.
-- For blocking operation inside subscribers, please use forkIO manually

-- Definition of a subscriber (internal to the EventBus, will be generated from Handler' when subscribing)
data Handler t = Handler {
  uuid :: UUID, -- Just a random UUID
  func :: EventBus t -> t -> IO () -- No UUID in function signature here
}
-- Definition of a subscriber function.
-- Will be curried to pass the UUID to the subscriber for unsubscribing
type Handler' t = UUID -> EventBus t -> t -> IO ()

-- The EventBus model
-- This model is one-time: Everytime the subscriber list changes, a new EventBus is created, inheriting the `chan` and `nextBus`
-- but with modified `handlers`. The new instance is now written to the TVar `nextBus` and will replace the old one on the next event.
data EventBus t = EventBus {
  chan :: TChan t, -- A TChan to send events to the bus
  handlers :: V.Vector (Handler t), -- Array of subscribers (can only change when switching to a new instance of EventBus)
  nextBus :: Maybe (TVar (EventBus t)) -- Point to the next value of this event bus
}

addHandler :: Handler t -> EventBus t -> EventBus t
addHandler handler bus@EventBus{..} = bus {
  handlers = V.snoc handlers handler
}

removeHandler :: UUID -> EventBus t -> EventBus t
removeHandler uid bus@EventBus{..} = bus {
  handlers = V.filter f handlers
} where f handler = uuid handler /= uid

setNextBus :: TVar (EventBus t) -> EventBus t -> EventBus t
setNextBus tvar bus@EventBus{..} = bus {
  nextBus = Just tvar
}

-- Initializes an EventBus
createBus :: IO (EventBus t)
createBus = do
  chan <- newTChanIO
  -- We have to bootstrap the EventBus: EventBus needs a nextBus but nextBus requires an existing EventBus
  -- First create one without nextBus
  let bus1 = EventBus chan V.empty Nothing
  -- Create a TVar pointing to the EventBus
  tvar <- newTVarIO bus1
  -- Change the nextBus property to the created TVar, which returns a new modified bus
  let bus2 = setNextBus tvar bus1
  -- Point the TVar to the modified EventBus
  atomically $ modifyTVar tvar (const bus2)
  return bus2

-- Start the EventBus in a Haskell thread
runBus :: EventBus t -> IO (EventBus t)
runBus bus = do
  _ <- async $ runBus' bus -- Don't wait for the async thread
  return bus

-- Actual code running inside the thread
runBus' :: EventBus t -> IO ()
runBus' bus = do
    -- Wait for event to come in
    ev <- atomically . readTChan $ chan bus
    -- The EventBus may have been modified (subscribed / unsubscribed)
    -- Fetch the current bus from nextBus
    myBus <- getNextBus bus
    --print $ V.length $ handlers myBus
    -- Call every subscriber for this event
    _ <- V.mapM (handle myBus ev) (handlers myBus)
    -- Recursively call itself. This is an infinite event loop.
    runBus' myBus
  where
    handle myBus ev handler = func handler myBus ev

getNextBusTVar :: EventBus t -> IO (TVar (EventBus t))
getNextBusTVar bus = return $ assertM' $ nextBus bus

getNextBus :: EventBus t -> IO (EventBus t)
getNextBus bus = readTVarIO . assertM' $ nextBus bus

-- Add a subscriber to EventBus
-- See the definition of Handler` for how to build a subscriber
subscribe :: EventBus t -> Handler' t -> IO UUID
subscribe bus handler = do
  uuid <- nextRandom
  -- Currying. Create a Handler from Handler', with UUID partially applied as the first parameter
  -- This enables unsubscribing because Handler also holds reference to the UUID
  let h = Handler uuid $ handler uuid
  newTVar <- getNextBusTVar bus
  -- Append the subscriber and point nextBus to the new one
  atomically $ modifyTVar newTVar $ addHandler h
  return uuid

-- Delete a subscriber from EventBus
-- Make use of the UUID passed to Handler'
unsubscribe :: EventBus t -> UUID -> IO ()
unsubscribe bus uid = do
  newTVar <- getNextBusTVar bus
  atomically $ modifyTVar newTVar $ removeHandler uid
  return ()

-- Send an event to EventBus
publish :: EventBus t -> t -> IO ()
publish bus ev = atomically $ writeTChan (chan bus) ev

-- Shorthands
subscribeOnce :: EventBus t -> Handler' t -> IO ()
subscribeOnce bus handler = do
  _ <- subscribe bus $ \uid b ev -> do
    handler uid b ev
    unsubscribe b uid
  return ()