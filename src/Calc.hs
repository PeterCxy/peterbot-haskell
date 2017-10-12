{-# LANGUAGE ViewPatterns #-}

module Calc where

import Control.Applicative (liftA2)
import Data.Char (isSpace, isDigit)
import Math.Gamma
import Text.Read (readEither)

data TokenType = Space | Digit | BasicOperator | Variable deriving (Eq, Enum)

-- Mathematical constants
constants :: [String]
constants = ["e", "pi", "π"]

-- Supported operators
operators :: [String]
operators = [
  "=", "+", "-", "*", "/", "^",
  "!",
  "sin", "cos", "tan", "sinh", "cosh", "tanh", "log"]

binaryOperators :: [String]
binaryOperators = ["=", "+", "-", "*", "/", "^"]
  
unaryOperators :: [String]
unaryOperators = ["sin", "cos", "tan", "sinh", "cosh", "tanh", "log", "!"]

brackets :: [String]
brackets = ["(", ")"]

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
  "tan" -> 4
  "sinh" -> 4
  "cosh" -> 4
  "tanh" -> 4
  "log" -> 4
  "!" -> 5
  _ -> -1000

binaryFunction :: String -> Either String (Double -> Double -> Double)
binaryFunction o = case o of
  "+" -> Right (+)
  "-" -> Right (-)
  "*" -> Right (*)
  "/" -> Right (/)
  "^" -> Right (**)
  _ -> Left $ "Unsupported operator " ++ o
  
unaryFunction :: String -> Either String (Double -> Double)
unaryFunction o = case o of
  "sin" -> Right sin
  "cos" -> Right cos
  "tan" -> Right tan
  "sinh" -> Right sinh
  "cosh" -> Right cosh
  "tanh" -> Right tanh
  "log" -> Right log
  "!" -> Right $ \n -> gamma $ n + 1
  _ -> Left $ "Unsupported operator " ++ o

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

leftAssociative  :: String -> Bool
leftAssociative o = case o of
  "^" -> False
  _ -> True

-- Convert (-x) to (0-x)
normalizeMinusNumbers :: String -> String
normalizeMinusNumbers str = reverse $ normalizeMinusNumbers' str '(' ""

normalizeMinusNumbers' :: String -> Char -> String -> String
normalizeMinusNumbers' "" _ res = res
normalizeMinusNumbers' ('-':str) '(' res = normalizeMinusNumbers' str '-' ('-':'0':res)
normalizeMinusNumbers' (' ':str) lastChar res = normalizeMinusNumbers' str lastChar (' ':res)
normalizeMinusNumbers' (c:str) _ res = normalizeMinusNumbers' str c (c:res)

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
infix2RPN e = fmap reverse $ infix2RPN' ((normalizeInfix $ normalizeMinusNumbers e) ++ " ") "" [] [] -- add a trailing space

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
infix2RPN' (c:e) (isOperator -> Just cur) opt opr =
  -- Make sure it doesn't form an operator with cur ++ [c]
  -- Because operators like sin and sinh share the same prefix
  case isOperator (cur ++ [c]) of
    Just newCur -> infix2RPN' e newCur opt opr
    Nothing -> let
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

-- Pop all the operators with greater precendence than the current one if the current one is left-associative
-- And push them to the output queue
popGreaterOperators :: String -> [String] -> [String] -> ([String], [String])
popGreaterOperators _ opt [] = (opt, []) -- empty
popGreaterOperators cur opt opr = let
    lastOperator = head opr
  in
    if (leftAssociative cur) && ((precedence cur) <= (precedence lastOperator))
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

calc :: String -> Either String Double
calc ex = do
    rpn <- r
    calcRPN rpn
  where
    r = case infix2RPN ex of
      Nothing -> Left $ "Parsing error (maybe mismatched parentheses?)"
      Just result -> Right result

-- Evaluate an expression with exactly one parameter x
eval1 :: String -> Double -> Either String Double
eval1 ex x = eval1_ ex $ show x

eval1_ :: String -> String -> Either String Double
eval1_ ex x = do
    rpn <- r
    calcRPN $ map f rpn
  where
    f token = if token == "x" then x else token
    r = case infix2RPN ex of
      Nothing -> Left $ "Parsing error (maybe mismatched parentheses?)"
      Just result -> Right result

-- Type used in calculation
-- Either an unparsed String, or a Double number
-- Because a List cannot hold different types of values
-- We have to unify the unparsed String (or the Strings containing operators) with Double
-- Otherwise we will have to convert between String and Double back and forth
-- Not to be confused with the return value, which is either en error or a Double result
type CalcToken = Either String Double

-- Convert double to CalcToken
-- Avoid the confusion of different 'Right's
tok :: Double -> CalcToken
tok = Right

isBinaryOperator :: CalcToken -> Maybe String
isBinaryOperator (Right _) = Nothing
isBinaryOperator (Left operator) =
  if elem operator binaryOperators
    then Just operator
    else Nothing

isUnaryOperator :: CalcToken -> Maybe String
isUnaryOperator (Right _) = Nothing
isUnaryOperator (Left operator) =
  if elem operator unaryOperators
    then Just operator
    else Nothing

isConstant :: CalcToken -> Maybe String
isConstant (Right _) = Nothing
isConstant (Left str) =
  if elem str constants
    then Just str
    else Nothing

-- Calculate the result of RPN (numbers only)
calcRPN :: [String] -> Either String Double
calcRPN rpn = calcRPN' (map Left rpn) []

calcRPN' :: [CalcToken] -> [CalcToken] -> Either String Double
-- calcRPN' rpn stack result = Just result
calcRPN' [] [] = Left "No result arising from the expression"
calcRPN' [] [r] = readEither'' r
calcRPN' [] _ = Left "Evaluation finished but there's item left in the stack"
calcRPN' ((isBinaryOperator -> Just o):_) [] = Left $ "Operator " ++ o ++ " needs two operands but none is provided."
calcRPN' ((isBinaryOperator -> Just o):_) (_:[]) = Left $ "Operator " ++ o ++ " needs two operands but only one is provided."
calcRPN' ((isBinaryOperator -> Just o):rpn) stack = do
    f <- binaryFunction o
    r' <- calculateBinary f ((head . tail) stack) (head stack)
    calcRPN' rpn ((tok r'):((tail . tail) stack))
calcRPN' ((isUnaryOperator -> Just o):_) [] = Left $ "Operator " ++ o ++ " needs one operand but none is provided."
calcRPN' ((isUnaryOperator -> Just o):rpn) stack = do
    f <- unaryFunction o
    r' <- calculateUnary f (head stack)
    calcRPN' rpn ((tok r'):(tail stack))
calcRPN' ((isConstant -> Just c):rpn) stack = do
    v <- val
    calcRPN' rpn ((tok v):stack)
  where
    val :: Either String Double
    val = case c of
      "e" -> Right $ exp 1
      "pi" -> Right pi
      "π" -> Right pi
      _ -> Left $ "Unsupported identifier " ++ c
calcRPN' (n:rpn) stack = calcRPN' rpn (n:stack)

calculateBinary :: (Double -> Double -> Double) -> CalcToken -> CalcToken -> Either String Double
calculateBinary op n1 n2 = do
  num1 <- readEither'' n1
  num2 <- readEither'' n2
  return $ op num1 num2

calculateUnary :: (Double -> Double) -> CalcToken -> Either String Double
calculateUnary op n = do
  num <- readEither'' n
  return $ op num

readEither' :: String -> Either String Double
readEither' str = case readEither str of
  Left _ -> Left $ "Illegal operand " ++ str
  Right res -> Right res

readEither'' :: CalcToken -> Either String Double
readEither'' t = case t of
  Left str -> readEither' str
  Right d -> Right d

instance Num b => Num (Either a b) where
  negate = fmap negate
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  fromInteger = pure . fromInteger
  abs = fmap abs
  signum = fmap signum

instance Fractional b => Fractional (Either a b) where
  fromRational = Right . fromRational
  (/) = liftA2 (/)

-- Note: the following functions are tailored for our need in this module
-- That is, the evaluation process return a Either String Double which may emit errors on execution

-- Calculate the derivative value at x of function f(x)
derivative1 :: (Double -> Either String Double) -> Double -> Either String Double
derivative1 f x = let
    h = 0.00000001
  in
    ((f $ x + h) - (f $ x - h)) / (2 * (Right h))

-- Solve f(x) = 0 by Newton's method
solveNewton :: (Double -> Either String Double) -> Int -> Double -> Either String Double
solveNewton _ 0 x = Right x
solveNewton f count x = do
  xn <- (Right x) - (f x) / (derivative1 f x)
  solveNewton f (count - 1) xn

solveNewton_ :: (Double -> Either String Double) -> Int -> String -> Either String Double
solveNewton_ f count x = do
  x' <- readEither' x
  solveNewton f count x'
