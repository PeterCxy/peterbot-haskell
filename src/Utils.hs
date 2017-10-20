{-# LANGUAGE OverloadedStrings, ViewPatterns, PatternSynonyms #-}

module Utils
  ( defVal
  , assertM
  , assertM'
  , ifM
  , generalize
  , liftMaybe
  , parseArgs
  , toQS
  , toBody
  ) where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import Data.ByteString
import Data.ByteString.Conversion
import qualified Data.Text as T
import Network.HTTP.Client
import Network.HTTP.Simple
import System.IO
import Types

defVal :: Maybe t -> t -> t
defVal Nothing def = def
defVal (Just val) _ = val

assertM :: String -> Maybe t -> t
assertM err Nothing = error err
assertM _ (Just val) = val

assertM' :: Maybe t -> t
assertM' = assertM "This should never happen"

ifM :: a -> Bool -> Maybe a
ifM a True = Just a
ifM _ False = Nothing

generalize :: Monad m => Identity a -> m a
generalize = return . runIdentity

-- Lift Maybe into MaybeT
-- Thus we can use Maybe inside MaybeT m
-- Just a combination of the MaybeT constructor and the return function
liftMaybe :: (Monad m) => Maybe a -> MaybeT m a
liftMaybe = MaybeT . return

-- Parse arguments passed to a bot command
--   /cmd_name arg1 arg2 "some argument with spaces" "I want \"quotation marks\" inside it!" arg5 arg6 ...
parseArgs :: T.Text -> [T.Text]
parseArgs str = Prelude.reverse $ parseArgs' (T.strip str) "" ' ' False []

-- Helper pattern synonyms
pattern StartWithSpace :: T.Text -> T.Text
pattern StartWithSpace t <- (T.stripPrefix " " -> Just t)
pattern StartWithQuote :: T.Text -> T.Text
pattern StartWithQuote t <- (T.stripPrefix "\"" -> Just t)

-- Actual implementation of the argument parser
-- Returns a revese list of arguments
parseArgs' :: T.Text -> T.Text -> Char -> Bool -> [T.Text] -> [T.Text]
-- parseArgs string currentItem lastChar insideQuote stackArray
-- Final case: the string becomes empty, we only need to push the final argument to list
-- Note that here we ignored the case that quotation marks do not match. This actually does not matter for our case.
parseArgs' "" cur _ _ arr = cur : arr
-- A space that is not wrapped inside any quotation mark
parseArgs' (StartWithSpace t) cur _ False arr = parseArgs' t "" ' ' False $ cur : arr
-- A quotation mark escaped by '\'
-- Drop the previous '\' and keep the quote
parseArgs' (StartWithQuote t) cur '\\' insideQuote arr = parseArgs' t (T.snoc (T.init cur) '"') '"' insideQuote arr
-- Beginning a quotation
parseArgs' (StartWithQuote t) "" _ False arr = parseArgs' t "" '"' True arr
-- Ending a quotation
parseArgs' (StartWithQuote t) cur _ True arr = parseArgs' t cur '"' False arr
-- Everything else: just take one char from str and put into cur
parseArgs' str cur _ insideQuote arr =
    parseArgs' (T.drop 1 str) (T.snoc cur c) c insideQuote arr
  where
    c = T.head str

-- Convert from (ByteString, something) to (ByteString, Maybe ByteString) which is required by HTTP's QueryString
toQS :: (ToByteString a) => [(ByteString, Maybe a)] -> [(ByteString, Maybe ByteString)]
toQS = Prelude.map (\tup -> (fst tup, fmap toByteString' $ snd tup))

-- Convert from (ByteString, something) to (ByteString, ByteString) which is required by HTTP's urlencoded body
toBody :: (ToByteString a) => [(ByteString, a)] -> [(ByteString, ByteString)]
toBody = Prelude.map (\tup -> (fst tup, toByteString' $ snd tup))
