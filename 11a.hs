{-# LANGUAGE RecordWildCards #-}

import AOC
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Functor ( ($>) )

main :: IO ()
main = interactg $ run . parseMonkeys

run :: M.Map Int Monkey -> Int
run = product .take 2 . reverse . sort . map (itemsInspected . snd) . M.toList . applyN 20 playRound

playRound :: M.Map Int Monkey -> M.Map Int Monkey
playRound map' = foldl' inspectItems map' monkeyList
    where 
        monkeyList = map fst $ M.toAscList map'

inspectItems :: M.Map Int Monkey -> Int -> M.Map Int Monkey
inspectItems map' key = foldl' (inspectAndThrowItem key) map' currentItems
    where
        fromMonkey =  map' M.! key
        currentItems = items fromMonkey

inspectAndThrowItem :: Int -> M.Map Int Monkey -> Int -> M.Map Int Monkey
inspectAndThrowItem fromMonkey map' item = M.adjust (\Monkey{..} -> Monkey{items=items ++ [boredLevel],..}) toMonkey $ M.adjust (\Monkey{..} -> Monkey{items= tail items, itemsInspected = itemsInspected + 1, ..}) fromMonkey map'
    where
        fromMonkey' = map' M.! fromMonkey
        worryLevel = increaseWorryLevel (operation fromMonkey') item
        toMonkey = throwItemToWhichMonkey fromMonkey' boredLevel
        boredLevel = worryLevel `div` 3

increaseWorryLevel :: Operation -> Int -> Int
increaseWorryLevel (Multiply (Literal int)) old = old * int
increaseWorryLevel (Multiply OldVar) old = old * old
increaseWorryLevel (Add (Literal int)) old = old + int
increaseWorryLevel (Add OldVar) old = old + old

throwItemToWhichMonkey :: Monkey -> Int -> Int
throwItemToWhichMonkey Monkey{test=DivisibleBy{..},..} worryLevel = if (worryLevel `rem` condition) == 0 then ifTrue else ifFalse 

data Monkey = Monkey {
    items :: ![Int],
    operation :: Operation,
    test :: Test,
    itemsInspected :: Int
} deriving (Show)

data Operation = Multiply Val | Add Val deriving (Show)
data Val = Literal Int | OldVar deriving (Show)

data Test = DivisibleBy {condition::Int, ifTrue:: Int, ifFalse::Int } deriving (Show)

parseMonkeys :: [[String]] -> M.Map Int Monkey
parseMonkeys str = M.fromAscList $ zip [0..] $ map parseMonkey str

parseMonkey :: [String] -> Monkey
parseMonkey [monkey, items', operation', test', ifTrue', ifFalse'] = Monkey{itemsInspected = 0, ..}
    where
        name = parse' (string "Monkey " *> integer) monkey
        items = parse' (string "  Starting items: " *> sepBy integer (string ", ")) items'
        operation = parse' (string "  Operation: new = old " *> (Multiply <$> (string "* " *> parseOperation) <|> (Add <$> (string "+ " *> parseOperation)))) operation'
        test = DivisibleBy (readLastWord test') (readLastWord ifTrue') (readLastWord ifFalse')

        readLastWord = read . last . splitOn " "

parseOperation :: Parser Val
parseOperation = (Literal <$> integer) <|> (string "old" $> OldVar)
