module XTL.Regex (matchCaptures) where

import XTL.String (packString, unpackString)
import Text.Regex.PCRE.Light (match, compile, Regex)

matchCaptures :: String -> String -> Maybe [String]
matchCaptures regexStr valueStr =
    let regex = compile (packString regexStr) []
        result = match regex (packString valueStr) []
    in case result of
        Just captures -> Just (map unpackString (drop 1 captures))
        _ -> Nothing
