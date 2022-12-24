{-# LANGUAGE NoImplicitPrelude #-}

import AOC
import Data.Char

main :: IO ()
main = interact' run

distinctChars :: Int
distinctChars = 14

run :: String -> Int
run str = distinctChars + (length . takeWhile (\s -> distinctChars /= (length . nub $ take distinctChars s)) $ tails str)
