-- {-# LANGUAGE OverloadedStrings #-}

import XTL.Regex (matchCaptures)
import XTL.Utils (getLeft, getRight)

import Control.Exception (IOException, try)
import Data.Either (isLeft, isRight)
import System.Console.Haskeline
import Text.Printf (printf)

data Operand = Distance Float | Time Float | Pace Float
data Operator = Add | Sub | Mul | Div | Pos | Neg
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
  where
    hours = read hourStr :: Float
    mins = read minStr :: Float
    secs = read secStr :: Float

timeToString :: Operand -> String
timeToString (Time t)
    | t < 0 = "-" ++ timeToString (Time (-t))
    | otherwise = show hours ++ ":" ++ printf "%02d" mins ++ ":" ++ printf "%02d" secs
  where
    t1 = round t :: Int
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
  where
    mins = read minStr :: Float
    secs = read secStr :: Float

paceToString :: Operand -> String
paceToString (Pace p) = show mins ++ "'" ++ printf "%05.2f" secs ++ "\""
  where
    t = floor p :: Int
    mins = t `div` 60
    secs = p - 60 * fromIntegral mins

asPace :: String -> Either Exception (Maybe Operand)
asPace s =
    let result = matchCaptures "^([0-9]+)\\'([0-9]+)(?:\\\")?$" s
     in case result of
            Just captures -> eitherToMaybeRight (paceFromString captures)
            _ -> Right Nothing

asOperand :: String -> Either Exception Operand
asOperand input =
    let tryFuncs [] = Left (Exception ("Invalid operand '" ++ input ++ "'"))
        tryFuncs (f : fs) =
            case f input of
                Left ex -> Left ex
                Right (Just v) -> Right v
                Right Nothing -> tryFuncs fs
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
evaluate (leftStr : operatorStr : rightStr : _)
    | isLeft leftOperand = leftOperand
    | isLeft operator = Left (Exception "TODO: Invalid operator")
    | isLeft rightOperand = rightOperand
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

----------------------------------------------------------------------------------------------------

data Token = TokenNumber Float | TokenPlus | TokenMinus | TokenStar | TokenSlash | TokenOpenPar | TokenClosePar | TokenEof

instance Show Token where
    show (TokenNumber value) = show value
    show TokenPlus = "+"
    show TokenMinus = "-"
    show TokenStar = "*"
    show TokenSlash = "/"
    show TokenOpenPar = "("
    show TokenClosePar = ")"
    show TokenEof = "EOF"

data TokenWithOffset = TokenWithOffset
    { token :: Token
    , offset :: Int
    }

tryTokenNumber :: String -> Maybe (Token, Int)
tryTokenNumber input =
    let result = matchCaptures "^([0-9]+(?:\\.[0-9]+)?)" input
     in case result of
            Just captures -> Just (TokenNumber (read tokenStr :: Float), length tokenStr)
              where
                tokenStr = head captures
            _ -> Nothing

tryTokenCharacter :: String -> Maybe (Token, Int)
tryTokenCharacter ('+' : _) = Just (TokenPlus, 1)
tryTokenCharacter ('-' : _) = Just (TokenMinus, 1)
tryTokenCharacter ('*' : _) = Just (TokenStar, 1)
tryTokenCharacter ('/' : _) = Just (TokenSlash, 1)
tryTokenCharacter ('(' : _) = Just (TokenOpenPar, 1)
tryTokenCharacter (')' : _) = Just (TokenClosePar, 1)
tryTokenCharacter _ = Nothing

readToken :: String -> Int -> Either Exception (TokenWithOffset, Int)
readToken input offset =
    let tryFuncs [] = Left (Exception ("Unexpected character at " ++ show offset ++ ": '" ++ input ++ "'"))
        tryFuncs (f : fs) =
            case f input of
                Just (token, tokenLen) -> Right (TokenWithOffset token offset, tokenLen)
                Nothing -> tryFuncs fs
     in tryFuncs [tryTokenNumber, tryTokenCharacter]

tokenizeImpl :: String -> Int -> Either Exception [TokenWithOffset]
tokenizeImpl [] _ = Right []
tokenizeImpl (' ' : rest) offset = tokenizeImpl rest (offset + 1)
tokenizeImpl input offset =
    let result = readToken input offset
     in case result of
            Left ex -> Left ex
            Right (tokenWithOffset, tokenLen) ->
                let subResult = tokenizeImpl (drop tokenLen input) (offset + tokenLen)
                 in case subResult of
                        Left ex -> Left ex
                        Right restTokens -> Right (tokenWithOffset : restTokens)

tokenize :: String -> Either Exception [TokenWithOffset]
tokenize input = tokenizeImpl input 0

data Lexeme = LexemeOperand Float | LexemeOperator Operator | LexemeOpen | LexemeClose | LexemeEof

instance Show Lexeme where
    show (LexemeOperand value) = show value
    show (LexemeOperator Add) = "+"
    show (LexemeOperator Sub) = "-"
    show (LexemeOperator Mul) = "*"
    show (LexemeOperator Div) = "/"
    show LexemeOpen = "("
    show LexemeClose = ")"
    show LexemeEof = "EOF"

type Stack = [Lexeme]
type Pool = [Float]

getStackTop :: Stack -> Lexeme
getStackTop (top : _) = top

{-
     | Stack
 In  | Opr | Mul | Div | Pos | Neg | Add | Sub |  (  |
-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
 Opr |  E  |  −  |  −  |  −  |  −  |  −  |  −  |  −  |  1  |
 Pos |  E  |  −  |  −  |  −  |  −  |  −  |  −  |  −  |  3  |
 Neg |  E  |  −  |  −  |  −  |  −  |  −  |  −  |  −  |  3  |
  (  |  E  |  −  |  −  |  −  |  −  |  −  |  −  |  −  |  3  |
 Mul |  +  |  +  |  +  |  −  |  −  |  −  |  −  |  −  |  5  |
 Div |  +  |  +  |  +  |  −  |  −  |  −  |  −  |  −  |  5  |
 Add |  +  |  +  |  +  |  +  |  +  |  +  |  +  |  −  |  7  |
 Sub |  +  |  +  |  +  |  +  |  +  |  +  |  +  |  −  |  7  |
  )  |  +  |  +  |  +  |  +  |  +  |  +  |  +  |  @  |  9  |
 Eof |  +  |  +  |  +  |  +  |  +  |  +  |  +  |  E  | 11  |
-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
     |  2  |  4  |  4  |  6  |  6  |  6  |  6  |  10 |
-}

{- FOURMOLU_DISABLE -}
getInputPriority :: Lexeme -> Int
getInputPriority (LexemeOperand _)    =  1
getInputPriority (LexemeOperator Pos) =  3
getInputPriority (LexemeOperator Neg) =  3
getInputPriority LexemeOpen           =  3
getInputPriority (LexemeOperator Mul) =  5
getInputPriority (LexemeOperator Div) =  5
getInputPriority (LexemeOperator Add) =  7
getInputPriority (LexemeOperator Sub) =  7
getInputPriority LexemeClose          =  9
getInputPriority LexemeEof            = 11
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
getStackPriority :: Lexeme -> Int
getStackPriority (LexemeOperand _)    =  2
getStackPriority (LexemeOperator Mul) =  4
getStackPriority (LexemeOperator Div) =  4
getStackPriority (LexemeOperator Pos) =  6
getStackPriority (LexemeOperator Neg) =  6
getStackPriority (LexemeOperator Add) =  6
getStackPriority (LexemeOperator Sub) =  6
getStackPriority LexemeOpen           = 10
{- FOURMOLU_ENABLE -}

popFromStack :: Stack -> Pool -> Either Exception (Stack, Pool)
popFromStack ((LexemeOperand value) : stackRest) pool = Right (stackRest, value : pool)
popFromStack ((LexemeOperator Add) : stackRest) (rhs : lhs : poolRest) = Right (stackRest, (lhs + rhs) : poolRest)
popFromStack ((LexemeOperator Sub) : stackRest) (rhs : lhs : poolRest) = Right (stackRest, (lhs - rhs) : poolRest)
popFromStack ((LexemeOperator Mul) : stackRest) (rhs : lhs : poolRest) = Right (stackRest, (lhs * rhs) : poolRest)
popFromStack ((LexemeOperator Div) : stackRest) (rhs : lhs : poolRest)
    | rhs == 0.0 = Left $ Exception "Division by zero"
    | otherwise = Right (stackRest, (lhs / rhs) : poolRest)
popFromStack ((LexemeOperator Pos) : stackRest) (op : poolRest) = Right (stackRest, op : poolRest)
popFromStack ((LexemeOperator Neg) : stackRest) (op : poolRest) = Right (stackRest, (-op) : poolRest)
popFromStack ((LexemeOperator Add) : _) _ = Left $ Exception "Not enough operands for operator +"
popFromStack ((LexemeOperator Sub) : _) _ = Left $ Exception "Not enough operands for operator -"
popFromStack ((LexemeOperator Mul) : _) _ = Left $ Exception "Not enough operands for operator *"
popFromStack ((LexemeOperator Div) : _) _ = Left $ Exception "Not enough operands for operator /"
popFromStack ((LexemeOperator Pos) : _) _ = Left $ Exception "Not enough operands for unary operator +"
popFromStack ((LexemeOperator Neg) : _) _ = Left $ Exception "Not enough operands for unary operator -"
popFromStack _ _ = Left (Exception "Internal error")

popFromStackAndContinue :: Lexeme -> Stack -> Pool -> Either Exception (Stack, Pool)
popFromStackAndContinue lexeme stack pool
    | isLeft result = result
    | isRight result =
        let (newStack, newPool) = getRight result
         in pushToStackAndContinue lexeme newStack newPool
  where
    result = popFromStack stack pool

pushToStackAndContinue :: Lexeme -> Stack -> Pool -> Either Exception (Stack, Pool)
pushToStackAndContinue LexemeClose [] pool = Left $ Exception "Unmatched closing bracket"
pushToStackAndContinue LexemeClose (LexemeOpen : stackRest) (poolTop : poolRest) = Right (LexemeOperand poolTop : stackRest, poolRest)
pushToStackAndContinue LexemeEof (LexemeOpen : _) pool = Left $ Exception "Unmatched opening bracket"
pushToStackAndContinue lexeme [] pool = Right ([lexeme], pool)
pushToStackAndContinue lexeme stack pool
    | inputPrio < stackPrio = Right (lexeme : stack, pool)
    | otherwise = popFromStackAndContinue lexeme stack pool
  where
    inputPrio = getInputPriority lexeme
    stackPrio = getStackPriority (getStackTop stack)

pushToStack :: TokenWithOffset -> Stack -> Pool -> Either Exception (Stack, Pool)
pushToStack (TokenWithOffset TokenEof offset) stack pool =
    case stack of
        (LexemeOperator op : _) -> Left $ Exception ("Not enough operands for operator " ++ show (LexemeOperator op))
        _ -> pushToStackAndContinue LexemeEof stack pool
pushToStack (TokenWithOffset (TokenNumber value) offset) stack pool =
    case stack of
        (LexemeOperand _ : _) -> Left $ Exception "Operator expected"
        _ -> pushToStackAndContinue (LexemeOperand value) stack pool
pushToStack (TokenWithOffset TokenPlus offset) stack pool =
    case stack of
        [] -> pushToStackAndContinue (LexemeOperator Pos) stack pool
        (LexemeOpen : _) -> pushToStackAndContinue (LexemeOperator Pos) stack pool
        (LexemeOperator _ : _) -> pushToStackAndContinue (LexemeOperator Pos) stack pool
        _ -> pushToStackAndContinue (LexemeOperator Add) stack pool
pushToStack (TokenWithOffset TokenMinus offset) stack pool =
    case stack of
        [] -> pushToStackAndContinue (LexemeOperator Neg) stack pool
        (LexemeOpen : _) -> pushToStackAndContinue (LexemeOperator Neg) stack pool
        (LexemeOperator _ : _) -> pushToStackAndContinue (LexemeOperator Neg) stack pool
        _ -> pushToStackAndContinue (LexemeOperator Sub) stack pool
pushToStack (TokenWithOffset TokenStar offset) stack pool = pushToStackAndContinue (LexemeOperator Mul) stack pool
pushToStack (TokenWithOffset TokenSlash offset) stack pool = pushToStackAndContinue (LexemeOperator Div) stack pool
pushToStack (TokenWithOffset TokenOpenPar offset) stack pool =
    case stack of
        (LexemeOperand _ : _) -> Left $ Exception "Operator expected"
        _ -> pushToStackAndContinue LexemeOpen stack pool
pushToStack (TokenWithOffset TokenClosePar offset) stack pool =
    case stack of
        (LexemeOperator op : _) -> Left $ Exception ("Not enough operands for operator " ++ show (LexemeOperator op))
        (LexemeOpen : _) -> Left $ Exception "Empty parentheses"
        _ -> pushToStackAndContinue LexemeClose stack pool

popFromPool :: [Float] -> Either Exception Float
popFromPool [] = Left (Exception "Pool is empty")
popFromPool [result] = Right result
popFromPool _ = Left (Exception "Multiple results in the pool")

evalTokens :: [TokenWithOffset] -> Stack -> Pool -> Either Exception Float
evalTokens (tokenWithOffset : tokensRest) stack pool
    | isLeft result = Left $ getLeft result
    | isRight result =
        let (newStack, newPool) = getRight result
         in evalTokens tokensRest newStack newPool
  where
    result = pushToStack tokenWithOffset stack pool
evalTokens [] stack pool = popFromPool pool

evalFinal :: String -> Either Exception Float
evalFinal input =
    let tokens = tokenize input
     in case tokens of
            Left ex -> Left ex
            Right tokens -> evalTokens (tokens ++ [TokenWithOffset TokenEof (length input)]) [] []

main :: IO ()
main = runInputT defaultSettings $ do
    outputStrLn "Welcome to Pace Calculator! Type 'q', 'quit', 'exit' or CTRL+D to quit."
    mainLoop
  where
    mainLoop :: InputT IO ()
    mainLoop = do
        -- let result = evalFinal "(8 / ) "
        -- case result of
        --     Right value -> outputStrLn (show value)
        --     Left (Exception what) -> outputStrLn what

        minput <- getInputLine "> "
        case minput of
            Nothing -> outputStrLn "Received EOF, goodbye!"
            Just "exit" -> outputStrLn "Goodbye!"
            Just "quit" -> outputStrLn "Goodbye!"
            Just "q" -> outputStrLn "Goodbye!"
            Just input -> do
                let result = evalFinal input
                case result of
                    Right value -> outputStrLn $ show value
                    Left (Exception what) -> outputStrLn what
                mainLoop

-- minput <- getInputLine "> "
-- case minput of
--     Nothing -> outputStrLn "Received EOF, goodbye!"
--     Just "exit" -> outputStrLn "Goodbye!"
--     Just "quit" -> outputStrLn "Goodbye!"
--     Just "q"    -> outputStrLn "Goodbye!"
--     Just input -> do
--         let result = parseAndEvaluate input
--         case result of
--             Right op -> outputStrLn (operandToString (Just op))
--             Left (Exception what) -> outputStrLn what
--         mainLoop
