module TestRegex (regexTests) where

import Test.Tasty
import Test.Tasty.HUnit
import XTL.Regex (matchCaptures)

-- Example module tests
regexTests :: TestTree
regexTests =
    testGroup
        "XTL.Regex tests"
        [ testCase "Empty capture" $ matchCaptures "(.*)" "" @?= Just [""]
        , testCase "Two empty captures" $ matchCaptures "(.*)(.*)" "" @?= Just ["", ""]
        , testCase "Integer" $ matchCaptures "(\\d+)" "-= 31415926 =-" @?= Just ["31415926"]
        , testCase "Not an integer" $ matchCaptures "(\\d+)" "-= Who let the DOGE out =-" @?= Nothing
        , testCase "Time H:MM:SS" $ matchCaptures "^(?:(\\d+):)?(\\d+):(\\d+)" "3:29:05 ..." @?= Just ["3", "29", "05"]
        , testCase "Time MM:SS" $ matchCaptures "^(?:(\\d+):)?(\\d+):(\\d+)" "19:38 ..." @?= Just ["", "19", "38"]
        ]
