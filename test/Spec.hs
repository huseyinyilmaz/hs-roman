module Main where

import Test.HUnit
import Data.Roman
testDigitValue :: Test
testDigitValue = TestCase $ do (assertEqual
                                "Test DigitValue 5"
                                (romanDigit2Int V)
                                5)
                               (assertEqual
                                "Test DigitValue 50"
                                (romanDigit2Int L)
                                50)


testRomanToInt::  Test
testRomanToInt = TestCase $ do (assertEqual
                                "Test value 50"
                                (romanToInt (Roman [L]))
                                50)
                               (assertEqual
                                 "Test 88"
                                 (romanToInt (Roman [L,X,X,X,V,I,I,I]))
                                 88)
                               (assertEqual
                                 "Test 99"
                                 (romanToInt (Roman [X,C,I,X]))
                                 99)

testStrToRoman:: Test
testStrToRoman = TestCase $ do (assertEqual
                                "Test value V"
                                (Roman [V])
                                (strToRoman "V"))
                               (assertEqual
                                "Test value XCIX"
                                (Roman [X,C,I,X])
                                (strToRoman "XCIX"))

testIntToRoman:: Test
testIntToRoman = TestCase $ do (assertEqual
                                "Test value 5"
                                (Roman [V])
                                (intToRoman 5))
                               (assertEqual
                                "Test value XCIX"
                                (Roman [X,C,I,X])
                                (intToRoman 99))


main :: IO Counts
main =  runTestTT $ TestList [testDigitValue, testRomanToInt, testStrToRoman, testIntToRoman]
