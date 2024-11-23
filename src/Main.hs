-- {-# LANGUAGE OverloadedStrings #-}

import XTL.Regex (matchCaptures)
import XTL.Utils (getLeft, getRight)

import Control.Exception (try, IOException)
import System.Console.Haskeline
import Data.Either (isLeft, isRight)
import Text.Printf (printf)


data Operand = Distance Float | Time Float | Pace Float
data Operator = Add | Sub | Mul | Div
data Exception = Exception String

asOperator :: String -> Either Exception Operator
asOperator s
    | s == "+" = Right Add
    | s == "-" = Right Sub
    | s == "*" = Right Mul
    | s == "/" = Right Div
    | otherwise = Left $ Exception ("Unknown operator " ++ s)

distanceFromString :: String -> Operand
distanceFromString s = Distance (read s :: Float)

distanceToString :: Operand -> String
distanceToString (Distance d) = show d ++ " km"

asDistance :: String -> Either Exception (Maybe Operand)
asDistance s =
    let result = matchCaptures "^([0-9]+(?:\\.[0-9]*)?)$" s
    in case result of
        Just captures -> Right (Just (distanceFromString (head captures)))
        _ -> Right Nothing

timeFromString :: [String] -> Either Exception Operand
timeFromString ["", minStr, secStr] = timeFromString ["0", minStr, secStr]
timeFromString [hourStr, minStr, secStr]
    | mins >= 60 = Left (Exception "Time: Invalid number of minutes")
    | secs >= 60 = Left (Exception "Time: Invalid number of seconds")
    | otherwise = Right (Time (60 * (60 * hours + mins) + secs))
    where hours = read hourStr :: Float
          mins = read minStr :: Float
          secs = read secStr :: Float

timeToString :: Operand -> String
timeToString (Time t)
    | t < 0 = "-" ++ (timeToString (Time (-t)))
    | otherwise = show hours ++ ":" ++ (printf "%02d" mins) ++ ":" ++ (printf "%02d" secs)
    where t1 = round t :: Int
          secs = t1 `mod` 60
          t2 = t1 `div` 60
          mins = t2 `mod` 60
          hours = t2 `div` 60

-- Convert Either Exception T to Either Exception (Maybe T)
eitherToMaybeRight :: Either e t -> Either e (Maybe t)
eitherToMaybeRight = fmap Just

asTime :: String -> Either Exception (Maybe Operand)
asTime s =
    let result = matchCaptures "^(?:([0-9]+):)?([0-9]+):([0-9]+)$" s
    in case result of
        Just captures -> eitherToMaybeRight (timeFromString captures)
        _ -> Right Nothing

paceFromString :: [String] -> Either Exception Operand
paceFromString [minStr, secStr]
    | secs >= 60 = Left (Exception "Pace: Invalid number of seconds")
    | otherwise = Right (Pace (60 * mins + secs))
    where mins = read minStr :: Float
          secs = read secStr :: Float

paceToString :: Operand -> String
paceToString (Pace p) = show mins ++ "'" ++ (printf "%05.2f" secs) ++ "\""
    where t = floor p :: Int
          mins = t `div` 60
          secs = p - 60 * (fromIntegral mins)

asPace :: String -> Either Exception (Maybe Operand)
asPace s =
    let result = matchCaptures "^([0-9]+)\\'([0-9]+)(?:\\\")?$" s
    in case result of
        Just captures -> eitherToMaybeRight (paceFromString captures)
        _ -> Right Nothing

asOperand :: String -> Either Exception Operand
asOperand input =
    let tryFuncs [] = Left (Exception ("Invalid operand '" ++ input ++ "'"))
        tryFuncs (f:fs) =
            case f input of
                Left ex        -> Left ex
                Right (Just v) -> Right v
                Right Nothing  -> tryFuncs fs
    in tryFuncs [asPace, asTime, asDistance]

operandToString :: Maybe Operand -> String
operandToString (Just (Time a)) = "Time: " ++ timeToString (Time a)
operandToString (Just (Pace a)) = "Pace: " ++ paceToString (Pace a)
operandToString (Just (Distance a)) = "Distance: " ++ distanceToString (Distance a)
operandToString Nothing = "???"

evaluateImpl :: Operand -> Operator -> Operand -> Either Exception Operand
evaluateImpl (Time t) Div (Pace p) = Right (Distance (t / p))
evaluateImpl (Time t) Div (Distance d) = Right (Pace (t / d))
evaluateImpl (Pace p) Mul (Distance d) = Right (Time (p * d))
evaluateImpl (Distance d) Mul (Pace p) = Right (Time (p * d))
evaluateImpl (Distance d1) Add (Distance d2) = Right (Distance (d1 + d2))
evaluateImpl (Distance d1) Sub (Distance d2) = Right (Distance (d1 - d2))
evaluateImpl (Time t1) Sub (Time t2) = Right (Time (t1 - t2))
evaluateImpl _ _ _ = Left (Exception "Cannot evaluate")

evaluate :: [String] -> Either Exception Operand
evaluate (leftStr:operatorStr:rightStr:_)
    | isLeft (leftOperand) = leftOperand
    | isLeft (operator) = Left (Exception "TODO: Invalid operator")
    | isLeft (rightOperand) = rightOperand
    | otherwise = evaluateImpl (getRight leftOperand) (getRight operator) (getRight rightOperand)
    where
        leftOperand = asOperand leftStr
        rightOperand = asOperand rightStr
        operator = asOperator operatorStr
evaluate _ = Left (Exception "Evaluation error")

parseAndEvaluate :: String -> Either Exception Operand
parseAndEvaluate input =
    let result = matchCaptures "^\\s*([0-9:\\'\"\\.]+)\\s*([\\+\\-\\*\\/])\\s*([0-9:\\'\"\\.]+)\\s*$" input
    in case result of
        Just captures -> evaluate captures
        _ -> Left (Exception "Could not parse the expression")


data Token = TokenOperand Float | TokenOperator Operator | TokenEof

instance Show Token where
    show (TokenOperand value) = show value
    show (TokenOperator Add) = "+"
    show (TokenOperator Sub) = "-"
    show (TokenOperator Mul) = "*"
    show (TokenOperator Div) = "/"

tokenPriority :: Token -> Int
tokenPriority (TokenOperand _) = 0
tokenPriority (TokenOperator Mul) = 1
tokenPriority (TokenOperator Div) = 1
tokenPriority (TokenOperator Add) = 2
tokenPriority (TokenOperator Sub) = 2
tokenPriority (TokenEof) = 10

popFromStack :: [Token] -> [Float] -> Either Exception ([Token], [Float])
popFromStack ((TokenOperand value):stackRest) pool = Right (stackRest, [value] ++ pool)
popFromStack ((TokenOperator Add):stackRest) (rhs:lhs:poolRest) = Right (stackRest, [lhs + rhs] ++ poolRest)
popFromStack ((TokenOperator Sub):stackRest) (rhs:lhs:poolRest) = Right (stackRest, [lhs - rhs] ++ poolRest)
popFromStack ((TokenOperator Mul):stackRest) (rhs:lhs:poolRest) = Right (stackRest, [lhs * rhs] ++ poolRest)
popFromStack ((TokenOperator Div):stackRest) (rhs:lhs:poolRest)
    | rhs == 0.0 = Left $ Exception "Division by zero"
    | otherwise  = Right (stackRest, [lhs / rhs] ++ poolRest)
popFromStack ((TokenOperator Add):stackRest) _ = Left (Exception "Not enough operands for operator +")
popFromStack ((TokenOperator Sub):stackRest) _ = Left (Exception "Not enough operands for operator -")
popFromStack ((TokenOperator Mul):stackRest) _ = Left (Exception "Not enough operands for operator *")
popFromStack ((TokenOperator Div):stackRest) _ = Left (Exception "Not enough operands for operator /")
popFromStack _ _ = Left (Exception "Internal error")

popFromStackAndContinue :: Token -> [Token] -> [Float] -> Either Exception ([Token], [Float])
popFromStackAndContinue token stack pool
    | isLeft result = result
    | isRight result =
        let (newStack, newPool) = getRight result
        in pushToStack token newStack newPool
    where
        result = popFromStack stack pool

type Stack = [Token]
type Pool = [Float]

-- pushToStackAndContinue :: Token -> Stack -> Pool -> Either Exception (Stack, Pool)

pushToStack :: Token -> Stack -> Pool -> Either Exception (Stack, Pool)
--pushToStack (TokenEof) (TokenOperator Add:_) pool = Left (Exception "Not enough operands for operator +")
--pushToStack (TokenEof) (TokenOperator Sub:_) pool = Left (Exception "Not enough operands for operator -")
--pushToStack (TokenEof) (TokenOperator Mul:_) pool = Left (Exception "Not enough operands for operator *")
--pushToStack (TokenEof) (TokenOperator Div:_) pool = Left (Exception "Not enough operands for operator /")
pushToStack token [] pool = Right ([token], pool)
pushToStack token (stackTop:stackRest) pool
    | tokenPrio == 0        = Right ([token, stackTop] ++ stackRest, pool)
    | tokenPrio < stackPrio = Right ([token, stackTop] ++ stackRest, pool)
    | otherwise = popFromStackAndContinue token ([stackTop] ++ stackRest) pool
    where
        tokenPrio = tokenPriority token
        stackPrio = tokenPriority stackTop

popFromPool :: [Float] -> Either Exception Float
popFromPool [] = Left (Exception "Pool is empty")
popFromPool [result] = Right result
popFromPool _ = Left (Exception "Multiple results in the pool")

evalTokens :: [Token] -> [Token] -> [Float] -> Either Exception Float
evalTokens (token:tokensRest) stack pool
    | isLeft result = Left (getLeft result)
    | isRight result =
        let (newStack, newPool) = getRight result
        in evalTokens tokensRest newStack newPool
    where
        result = pushToStack token stack pool
evalTokens [] stack pool = popFromPool pool

evalThem :: [Token] -> Either Exception Float
evalThem tokens = evalTokens (tokens ++ [TokenEof]) [] []

evalFinal :: String -> Either Exception Float
evalFinal input =
    let tokens = tokenize input
    in case tokens of
        Left ex -> Left ex
        Right tokens -> evalThem tokens

asTokenOperand :: String -> Maybe (Token, Int)
asTokenOperand input =
    let result = matchCaptures "^([0-9]+(?:\\.[0-9]+)?)" input
    in case result of
        Just captures -> Just $ (TokenOperand (read tokenStr :: Float), length tokenStr)
            where
                tokenStr = head captures
        _ -> Nothing

asTokenOperator :: String -> Maybe (Token, Int)
asTokenOperator ('+':rest) = Just $ (TokenOperator Add, 1)
asTokenOperator ('-':rest) = Just $ (TokenOperator Sub, 1)
asTokenOperator ('*':rest) = Just $ (TokenOperator Mul, 1)
asTokenOperator ('/':rest) = Just $ (TokenOperator Div, 1)
asTokenOperator _ = Nothing

popToken :: String -> Either Exception (Token, Int)
popToken input =
    let tryFuncs [] = Left (Exception ("Unexpected character '" ++ input ++ "'"))
        tryFuncs (f:fs) =
            case f input of
                Just (token, len) -> Right (token, len)
                Nothing    -> tryFuncs fs
    in tryFuncs [asTokenOperand, asTokenOperator]



tokenize :: String -> Either Exception [Token]
tokenize [] = Right []
tokenize (' ':rest) = tokenize rest
tokenize input =
    let result = popToken input
    in case result of
        Left ex -> Left ex
        Right (token, tokenLen) ->
            let subResult = tokenize (drop tokenLen input)
            in case subResult of
                Left ex -> Left ex
                Right restTokens -> Right ([token] ++ restTokens)





main :: IO ()
main = runInputT defaultSettings $ do
        outputStrLn "Welcome to Pace Calculator! Type 'q', 'quit', 'exit' or CTRL+D to quit."
        mainLoop
    where
        mainLoop :: InputT IO ()
        mainLoop = do
            let result = evalFinal "0.33 + 3 / 4 + 0.12 * 8 / 0"
            case result of
                Right value -> outputStrLn (show value)
                Left (Exception what) -> outputStrLn what

            minput <- getInputLine "> "
            case minput of
                Nothing -> outputStrLn "Received EOF, goodbye!"
                Just "exit" -> outputStrLn "Goodbye!"
                Just "quit" -> outputStrLn "Goodbye!"
                Just "q"    -> outputStrLn "Goodbye!"
                Just input -> do
                    let result = parseAndEvaluate input
                    case result of
                        Right op -> outputStrLn (operandToString (Just op))
                        Left (Exception what) -> outputStrLn what
                    mainLoop
