{-# LANGUAGE NoImplicitPrelude #-}

import AOC
import qualified Data.Map as M
import Algorithm.Search
import Data.Char

main :: IO ()
main = interact $ run . parseGrid head

run str = length $ fromJust finalState
    where 
        elevationMap = tupleMap str
        finalMap = M.adjust (const 'z') (fst end) $ M.adjust (const 'a') (fst start) elevationMap
        finalState =  bfs (getNeighbors finalMap) (\x ->x == (fst end)) (fst start)
        start = head $ M.toList $ M.filter (== 'S') elevationMap
        end = head $ M.toList $ M.filter (=='E') elevationMap

getNeighbors :: M.Map (Int,Int) Char -> (Int, Int) -> [(Int,Int)]
getNeighbors m p =
    filter (\x -> case (m M.!? p, m M.!? x ) of
        (Just p, Just x ) -> ord x - ord p  <= 1
        _ -> False) $ neighbours p
    where
        neighbours (x, y) = [(x + xn, y + yn) | xn <- [-1..1], yn <- [-1..1], xn /= yn && xn /= (-1) * yn]

isValidStep :: Char -> Char -> Bool
isValidStep c1 c2 = ord c1 - ord c2 >= -1

tupleMap :: [[Char]] -> M.Map (Int, Int) Char
tupleMap xss = M.fromList $ do
  (y, row) <- coordinates
  (x, nr) <- row
  pure ((x, y), nr)
  where
    coordinates = zip [0 ..] $ zip [0 ..] <$> xss



