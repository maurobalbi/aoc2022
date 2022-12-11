{-# LANGUAGE NoImplicitPrelude #-}

import AOC
import Data.Char

main :: IO ()
main = interact $ f

f :: Foldable t => t String -> Int
f xs = foldr ((+) . intersectValue) 0 xs

intersectValue :: String -> Int
intersectValue = charsToValue . nub . intersectBackpack

intersectBackpack:: String -> String
intersectBackpack str = (xs1 `intersect` xs2)
  where
    halfLength = (length str) `div` 2
    xs1 = take halfLength str
    xs2 = drop halfLength str

charsToValue :: String -> Int
charsToValue = sum . map charToValue

charToValue :: Char -> Int
charToValue x | isUpper x = ord x - 38
charToValue x = ord x - 96