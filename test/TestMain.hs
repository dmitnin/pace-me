module Main where

import Test.Tasty
import TestRegex (regexTests)

main :: IO ()
main =
    defaultMain $
        testGroup
            "All Tests"
            [ regexTests
            -- , moduleTests
            ]
