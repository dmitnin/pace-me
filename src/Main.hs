-- {-# LANGUAGE OverloadedStrings #-}

import Text.Regex.PCRE.Light (match, compile, Regex)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE  -- For encoding Text to ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import Data.ByteString (ByteString)
import Data.Either (isLeft)
import System.IO (hFlush, stdout)
import Text.Printf (printf)


packString :: String -> BS.ByteString
packString = TE.encodeUtf8 . T.pack

unpackString :: BS.ByteString -> String
unpackString = T.unpack . TE.decodeUtf8

-- Safe head function
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

-- Unpack ByteString to Text
unpack :: Maybe ByteString -> T.Text
unpack (Just bs) = TE.decodeUtf8 bs
unpack Nothing   = T.pack ""

--checkMatch :: T.Text -> Regex -> Maybe String
--checkMatch input regex = 
--    case match regex (TE.encodeUtf8 input) [] of  -- Convert Text to ByteString
--        Just captures -> evaluate (map unpackString (drop 1 captures))
--        Nothing       -> Nothing

data Operand = Distance Float | Time Float | Pace Float
data Operator = Mul | Div | Unknown

asOperator :: String -> Either Exception Operator
asOperator s
    | s == "*" = Right Mul
    | s == "/" = Right Div
    | otherwise = Left $ Exception ("Unknown operator " ++ s)

-- parse Operand :: String -> Maybe Operand

matchCaptures :: String -> String -> Maybe [String]
matchCaptures valueStr regexStr =
    let regex = compile (packString regexStr) []
        result = match regex (packString valueStr) []
    in case result of
        Just captures -> Just (map unpackString (drop 1 captures))
        _ -> Nothing

distanceFromString :: String -> Operand
distanceFromString s = Distance (read s :: Float)

asDistance :: String -> Either Exception (Maybe Operand)
asDistance s =
    let result = matchCaptures s "^([0-9]+(?:\\.[0-9]*)?)$"
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
timeToString (Time t) = show hours ++ ":" ++ (printf "%02d" mins) ++ ":" ++ (printf "%02d" secs)
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
    let result = matchCaptures s "^(?:([0-9]+):)?([0-9]+):([0-9]+)$"
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
    let result = matchCaptures s "^([0-9]+)\\'([0-9]+)(?:\\\")?$"
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
operandToString (Just (Distance a)) = "Distance: " ++ show a
operandToString Nothing = "???"

evaluateImpl :: Operand -> Operator -> Operand -> Either Exception Operand
evaluateImpl (Time t) Div (Pace p) = Right (Distance (t / p))
evaluateImpl (Time t) Div (Distance d) = Right (Pace (t / d))
evaluateImpl (Pace p) Mul (Distance d) = Right (Time (p * d))
evaluateImpl (Distance d) Mul (Pace p) = Right (Time (p * d))
evaluateImpl _ _ _ = Left (Exception "Cannot evaluate")

getLeft :: Either a b -> a
getLeft (Left x) = x

getRight :: Either a b -> b
getRight (Right x) = x

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
    let result = matchCaptures input "^\\s*([0-9:\\'\"\\.]+)\\s*([\\*\\/])\\s*([0-9:\\'\"\\.]+)\\s*$"
    in case result of
        Just captures -> evaluate captures
        _ -> Left (Exception "Could not parse the expression")


mainLoop :: IO ()
mainLoop = do
    putStr "> "
    hFlush stdout  -- Ensures the prompt is shown before user input

    -- Read the user's input
    input <- getLine

    -- If the user types "exit", stop the loop
    if input == "exit"
        then putStrLn "Goodbye!"  -- Print a goodbye message
        else do
            let result = parseAndEvaluate input
            case result of
                Right op -> putStrLn (operandToString (Just op))
                Left (Exception what) -> putStrLn what

            mainLoop  -- Call the function recursively to continue

main :: IO ()
main = do
    putStrLn "Pace Maker: Type 'exit' to quit."
    mainLoop

    let result = parseAndEvaluate "29:45 / 5.05"
    case result of
        Right op -> putStrLn (operandToString (Just op))
        Left (Exception what) -> putStrLn what
