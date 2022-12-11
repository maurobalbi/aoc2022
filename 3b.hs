{-# LANGUAGE NoImplicitPrelude #-}

import AOC
import Data.Char
import Data.List.Split

main :: IO ()
main = interact $ f

f :: [String] -> Int
f xs = foldr ((+) . charsToValue . intersectChunks) 0 $ chunksOf 3 xs

intersectChunks :: [String] -> String
intersectChunks = nub . foldr1 intersect

intersectValue :: String -> Int
intersectValue = charsToValue . nub

charsToValue :: String -> Int
charsToValue = sum . map charToValue

charToValue :: Char -> Int
charToValue x | isUpper x = ord x - 38
charToValue x = ord x - 96