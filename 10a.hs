{-# LANGUAGE NoImplicitPrelude #-}

import AOC
import qualified Data.Set as S

main :: IO ()
main = interact $ eval . parseInput

data Instruction = Noop | Addx Int deriving (Eq, Show)

every n xs = case xs of
              y : ys -> y : (every n $ drop (n -1) ys)
              [] -> []

eval ::[Instruction] -> Int
eval instr = sum $ map (\(a,b) -> a * b) $  every 40 $ drop 19 $ zip [1..] $ eval' 1 instr
    where 
        eval' val (Noop:rest) = val:(eval' val rest)
        eval' val (Addx i:rest) = (val) : (eval' (val + i) rest)
        eval' val [] = []

parseInput :: [String] -> [Instruction]
parseInput = join . parseList p

p :: Parser [Instruction]
p = string "noop" *> pure [Noop] 
    <|> do
        string "addx "
        si <- signedInteger
        pure $ [Noop, Addx si]


