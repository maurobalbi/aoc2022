{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

import AOC
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Functor hiding (map)
import Data.Foldable hiding (map)

main :: IO ()
main = interactg $ run . parseMonkeys

run :: M.Map Int Monkey -> M.Map Int Monkey
run = execState playRound

playRound :: State (M.Map Int Monkey) ()
playRound = do
    map_ <- get
    let list = M.toAscList map_
    traverse_ (inspectItems . fst) list

inspectItems :: Int -> State (M.Map Int Monkey) ()
inspectItems key = do
    monkey <- gets ( M.lookup key)
    let items' = maybe [] items monkey
    traverse_ (inspectAndThrowItem key) items'


inspectAndThrowItem :: Int -> Int -> State (M.Map Int Monkey) ()
inspectAndThrowItem fromMonkey item = do
    m@Monkey{..} <- gets (fromMaybe (error "No monkey") . M.lookup fromMonkey)
    let worryLevel = increaseWorryLevel (operation) item
    let boredLevel = worryLevel `div` 3
    let toMonkey = throwItemToWhichMonkey m boredLevel
    modify (M.adjust (\Monkey{..} -> Monkey{items = items ++ [boredLevel],..}) toMonkey . M.adjust (\Monkey{..} -> Monkey{items= tail items, ..}) fromMonkey)
    pure ()

increaseWorryLevel :: Operation -> Int -> Int
increaseWorryLevel (Multiply (Literal int)) old = old * int
increaseWorryLevel (Multiply OldVar) old = old * old
increaseWorryLevel (Add (Literal int)) old = old + int
increaseWorryLevel (Add OldVar) old = old + old

throwItemToWhichMonkey :: Monkey -> Int -> Int
throwItemToWhichMonkey Monkey{test=DivisibleBy{..},..} worryLevel = if (worryLevel `mod` condition) == 0 then ifTrue else ifFalse 

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
