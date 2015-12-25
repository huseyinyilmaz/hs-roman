{-
Roman numeral implementation in haskell
-}
module Data.Roman where

import Data.List(group)

data RomanDigit = I | V | X | L | C | D | M deriving (Show, Eq, Ord, Enum)
data Roman = Roman [RomanDigit] deriving (Eq, Ord)

instance Show Roman where
  show (Roman ds) = concat (map show ds)

romanDigit2Int :: RomanDigit -> Int
romanDigit2Int I = 1
romanDigit2Int V = 5
romanDigit2Int X = 10
romanDigit2Int L = 50
romanDigit2Int C = 100
romanDigit2Int D = 500
romanDigit2Int M = 1000

charToRomanDigit:: Char -> RomanDigit
charToRomanDigit 'I' = I
charToRomanDigit 'V' = V
charToRomanDigit 'X' = X
charToRomanDigit 'L' = L
charToRomanDigit 'C' = C
charToRomanDigit 'D' = D
charToRomanDigit 'M' = M
charToRomanDigit s = error ("Cannot parse digit: " ++ [s])


strToRoman:: String -> Roman
strToRoman s = Roman $ fmap charToRomanDigit s


romanToInt::Roman -> Int
romanToInt (Roman rs)= (sum.gd.(map sum).group.(map romanDigit2Int)) rs
  where
    gd :: [Int] -> [Int]
    gd [] = [0]
    gd [i] = [i]
    gd (i:i':is) | i<i' = ((i' - i): (gd is))
                 | otherwise = i : gd (i':is)


intToRoman :: Int -> Roman
intToRoman i | i >= 1000 = error "Cannot convert number larger than 1000"
             |otherwise = Roman $ i2r i digits
  where
    digits = [(I)..]
    i2r 0 _ = []
    i2r i (one:five:ten:ds) = i2r num (ten:ds) ++ (numToRoman digit)
      where
        digit = i `mod` 10
        num = i `div` 10
        numToRoman 1 = [one]
        numToRoman 2 = [one, one]
        numToRoman 3 = [one, one, one]
        numToRoman 4 = [one, five]
        numToRoman 5 = [five]
        numToRoman 6 = [five, one]
        numToRoman 7 = [five, one, one]
        numToRoman 8 = [five, one, one, one]
        numToRoman 9 = [one, ten]
        numToRoman 0 = []
