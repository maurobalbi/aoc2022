{-# LANGUAGE NoImplicitPrelude #-}

import AOC
import qualified Data.Map as M
import qualified Data.Map.Merge.Strict as M
import Data.IntMap.Merge.Lazy (traverseMaybeMissing, zipWithAMatched)

main :: IO ()
main = interact $ f . parseList p

type Point = (Int,Int)
data PointType = Sand | Wall deriving (Eq, Show, Ord)

f ps = M.size $ M.filter (== Sand) sandMap
    where
        sandMap = converge (stepTime max) $ M.insert (500, 0) Sand pointMap
        pointMap = foldr1 M.union $ map addPoints ps
        max = maximum $  map snd $ M.keys pointMap

stepTime :: Int -> M.Map Point PointType -> M.Map Point PointType
stepTime max map = M.foldrWithKey (insertSand max) map map 

insertSand :: Int -> Point -> PointType -> M.Map Point PointType ->  M.Map Point PointType
insertSand maxY (x,y) Sand b =
    if y > maxY then b
    else M.insertWith max (x - 1, y + 1) Sand $  M.insertWith max (x + 1, y + 1) Sand $ M.insertWith max (x, y+1) Sand b
insertSand max p _ b = b

addPoints xs = M.fromList $ do
    ((x1, y1), (x2, y2)) <- pairs
    a <- range x1 x2
    b <- range y1 y2
    pure ((a,b), Wall)
    where
        pairs = zip xs (drop 1 xs)

range a b = if a < b then [a..b] else [b..a]

p :: Parser [Point]
p = sepBy1 
        (do
            p1 <- integer
            char ','
            p2 <- integer
            pure (p1, p2)
        ) 
        (string " -> ")







