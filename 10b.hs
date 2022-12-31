{-# LANGUAGE NoImplicitPrelude #-}

import AOC
import qualified Data.Set as S
import Prelude

main :: IO ()
main = Prelude.interact $ (++ "\n") . printCRT . parseInput . lines

data Instruction = Noop | Addx Int deriving (Eq, Show)

group' :: Int -> [a] -> [[a]]
group' _ [] = []
group' n l
  | n > 0 = (take n l) : (group' n (drop n l))
  | otherwise = error "Negative or zero n"

printCRT :: [Instruction] -> String
printCRT = (++) "\n" . unlines . group' 40 . map (\(cycle,reg) -> if cycle `mod` 40 >= (reg - 1) && cycle `mod` 40 <= (reg + 1) then '#' else ' ') . eval

eval ::[Instruction] -> [(Int,Int)]
eval instr = zip [0..] $ eval' 1 instr
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


