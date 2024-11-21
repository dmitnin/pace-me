-- {-# LANGUAGE OverloadedStrings #-}

import XTL.Regex (matchCaptures)
import XTL.Utils (getRight)

import Data.Either (isLeft)
import System.IO (hFlush, stdout)
import Text.Printf (printf)
import Control.Exception (try, IOException)


data Operand = Distance Float | Time Float | Pace Float
data Operator = Add | Sub | Mul | Div | Unknown

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

data Exception = Exception String

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


mainLoop :: IO ()
mainLoop = do
    putStr "> "
    hFlush stdout  -- Ensures the prompt is shown before user input

    input <- try getLine :: IO (Either IOException String)

    case input of
        Left _ -> putStrLn "\nReceived EOF, goodbye!"
        Right input -> do
            if input == "exit"
                then putStrLn "Goodbye!"
                else do
                    let result = parseAndEvaluate input
                    case result of
                        Right op -> putStrLn (operandToString (Just op))
                        Left (Exception what) -> putStrLn what

                    mainLoop  -- Call the function recursively to continue

main :: IO ()
main = do
    putStrLn "Pace Calculator: Type 'exit' to quit."
    mainLoop

    -- let result = parseAndEvaluate "29:45 / 5.05"
    -- case result of
    --     Right op -> putStrLn (operandToString (Just op))
    --     Left (Exception what) -> putStrLn what
