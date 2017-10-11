{-# LANGUAGE ViewPatterns #-}

module Calc where

import Data.Char (isSpace, isDigit)
import Math.Gamma
import Text.Read (readEither)

data TokenType = Space | Digit | BasicOperator | Variable deriving (Eq, Enum)

-- Supported operators
operators :: [String]
operators = ["=", "+", "-", "*", "/", "^", "!", "sin", "cos"]

brackets :: [String]
brackets = ["(", ")"]

getTokenType :: Char -> TokenType
getTokenType c
  | c == ' ' = Space
  | elem [c] (operators ++ ["(", ")"]) = BasicOperator -- Only one-char operators (count parentheses as operators)
  | isDigit c = Digit
  | c == '.' = Digit -- Decimals
  | otherwise = Variable

isOperator :: String -> Maybe String
isOperator operator =
  if elem operator operators
    then Just operator
    else Nothing

notOperator :: String -> Maybe String
notOperator operator =
  if (elem operator operators) || (elem operator brackets)
    then Nothing
    else Just operator 

precedence :: String -> Int
precedence o = case o of
  "=" -> 0
  "+" -> 1
  "-" -> 1
  "*" -> 2
  "/" -> 2
  "^" -> 3
  "sin" -> 4
  "cos" -> 4
  "!" -> 5
  _ -> -1000

leftAssociative  :: String -> Bool
leftAssociative o = case o of
  "^" -> False
  _ -> True

-- Normalize any infix expression to something needed by the function infix2RPN
normalizeInfix :: String -> String
normalizeInfix str = reverse $ normalizeInfix' str Space ""

normalizeInfix' :: String -> TokenType -> String -> String
-- normalizeInfix' str last_char_type ret_val
normalizeInfix' "" _ r = r -- Last case
normalizeInfix' (' ':str) Space r = normalizeInfix' str Space r -- Discard repetitive spaces
normalizeInfix' (c:str) Space r = normalizeInfix' str (getTokenType c) (c:r) -- Don't count space as a different character type
normalizeInfix' (c:str) lastType r =
  let
    tokenType = getTokenType c
  in
    if tokenType == lastType
      then normalizeInfix' str tokenType (c:r)
      else normalizeInfix' str tokenType (c:' ':r) -- Insert a space between two tokens with different types

-- Convert infix notation to reverse-polish (RPN) by shutting-yard algorithm
-- normalize before passing to the infix2RPN' which does the actual job
infix2RPN :: String -> Maybe [String]
infix2RPN e = fmap reverse $ infix2RPN' ((normalizeInfix e) ++ " ") "" [] [] -- add a trailing space

-- Convert infix notation to reverse-polish (RPN) by shutting-yard algorithm
-- This requires different types of tokens to be separated by spaces
-- e.g. 1 + 2 -( 4 - 5 ^ 2 )
-- and we need a trailing space
infix2RPN' :: String -> String -> [String] -> [String] -> Maybe [String]
-- infix2RPN' expression current_token output_queue operator_stack = result
-- return Nothing if failed to parse
-- if nothing to read, pop everything inside the operator stack
infix2RPN' "" cur opt opr = popEverything (cur:opt) opr
-- push to the output queue if there is a space and we are not on an operator (this happens, for example, at the end of the expression)
-- if the current character is an operator, jump to the operator case (which does more than this)
infix2RPN' (' ':e) (notOperator -> Just cur) opt opr = infix2RPN' e "" (cur:opt) opr
-- An operator!
-- Pop everything that has a greater precedence than itself
-- then push itself to the operator stack
infix2RPN' (c:e) (isOperator -> Just cur) opt opr = let
    newParams = popGreaterOperators cur opt opr
  in
    infix2RPN' e [c] (fst newParams) (cur:(snd newParams)) -- Push the current operator into the operator stack
-- Left bracket. Just push it.
infix2RPN' (c:e) "(" opt opr = infix2RPN' e [c] opt ("(":opr) -- Push the left bracket into the operator stack
-- Right bracket! Pop until left. If no left is found, then return Nothing
infix2RPN' (c:e) ")" opt opr = do
    newParams <- popUntilLeft opt opr
    infix2RPN' e [c] (fst newParams) (snd newParams)
-- Discard spaces on queue
infix2RPN' (c:e) " " opt opr = infix2RPN' e [c] opt opr
-- Everything else
infix2RPN' (c:e) cur opt opr = infix2RPN' e (cur ++ [c]) opt opr

-- Pop all the left-associative operators with greater precendence than the current one
-- And push them to the output queue
popGreaterOperators :: String -> [String] -> [String] -> ([String], [String])
popGreaterOperators _ opt [] = (opt, []) -- empty
popGreaterOperators cur opt opr = let
    lastOperator = head opr
  in
    if (leftAssociative lastOperator) && ((precedence cur) <= (precedence lastOperator))
      then popGreaterOperators cur (lastOperator:opt) (tail opr)
      else (opt, opr)

-- Pop every operator until hitting a left bracket
popUntilLeft :: [String] -> [String] -> Maybe ([String], [String])
popUntilLeft _ [] = Nothing -- No left brackets!
popUntilLeft opt opr = let
    lastOperator = head opr
  in
    if lastOperator == "("
      then Just (opt, tail opr)
      else popUntilLeft (lastOperator:opt) (tail opr)

-- Pop everything
popEverything :: [String] -> [String] -> Maybe [String]
popEverything opt [] = Just $ filter (not . (all isSpace)) opt
popEverything opt opr = let
    lastOperator = head opr
  in
    if lastOperator == "(" -- Mismatched parentheses
      then Nothing
      else popEverything (lastOperator:opt) (tail opr)

binaryOperators :: [String]
binaryOperators = ["=", "+", "-", "*", "/", "^"]

unaryOperators :: [String]
unaryOperators = ["sin", "cos", "!"]

isBinaryOperator :: String -> Maybe String
isBinaryOperator operator =
  if elem operator binaryOperators
    then Just operator
    else Nothing

isUnaryOperator :: String -> Maybe String
isUnaryOperator operator =
  if elem operator unaryOperators
    then Just operator
    else Nothing

calc :: String -> Either String Double
calc ex = do
    rpn <- r
    calcRPN rpn
  where
    r = case infix2RPN ex of
      Nothing -> Left $ "Parsing error (maybe mismatched parentheses?)"
      Just result -> Right result

-- Calculate the result of RPN (numbers only)
calcRPN :: [String] -> Either String Double
calcRPN rpn = calcRPN' rpn []

calcRPN' :: [String] -> [String] -> Either String Double
-- calcRPN' rpn stack result = Just result
calcRPN' [] [] = Left "No result arising from the expression"
calcRPN' [] [r] = readEither' r
calcRPN' [] _ = Left "Evaluation finished but there's item left in the stack"
calcRPN' ((isBinaryOperator -> Just o):_) [] = Left $ "Operator " ++ o ++ " needs two operands but none is provided."
calcRPN' ((isBinaryOperator -> Just o):_) (_:[]) = Left $ "Operator " ++ o ++ " needs two operands but only one is provided."
calcRPN' ((isBinaryOperator -> Just o):rpn) stack = do
    f <- func
    r' <- calculateBinary f ((head . tail) stack) (head stack)
    calcRPN' rpn ((show r'):((tail . tail) stack))
  where
    func :: Either String (Double -> Double -> Double)
    func = case o of
      "+" -> Right (+)
      "-" -> Right (-)
      "*" -> Right (*)
      "/" -> Right (/)
      "^" -> Right (**)
      _ -> Left $ "Unsupported operator " ++ o
calcRPN' ((isUnaryOperator -> Just o):_) [] = Left $ "Operator " ++ o ++ " needs one operand but none is provided."
calcRPN' ((isUnaryOperator -> Just o):rpn) stack = do
    f <- func
    r' <- calculateUnary f (head stack)
    calcRPN' rpn ((show r'):(tail stack))
  where
    func :: Either String (Double -> Double)
    func = case o of
      "sin" -> Right sin
      "cos" -> Right cos
      "!" -> Right $ \n -> gamma $ n + 1
      _ -> Left $ "Unsupported operator " ++ o
calcRPN' (n:rpn) stack = calcRPN' rpn (n:stack)

calculateBinary :: (Double -> Double -> Double) -> String -> String -> Either String Double
calculateBinary op n1 n2 = do
  num1 <- readEither' n1
  num2 <- readEither' n2
  return $ op num1 num2

calculateUnary :: (Double -> Double) -> String -> Either String Double
calculateUnary op n = do
  num <- readEither' n
  return $ op num

readEither' :: String -> Either String Double
readEither' str = case readEither str of
  Left _ -> Left $ "Illegal operand " ++ str
  Right res -> Right res