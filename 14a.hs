{-# LANGUAGE NoImplicitPrelude #-}

import AOC
import qualified Data.Set as S

main :: IO ()
main = interact $ f . parseList p

type Point = (Int,Int)

f ps = S.size (fst sandMap) - S.size pointMap
    where
        sandMap = converge (stepTime max) (pointMap, (500,0))
        pointMap = foldr1 S.union $ map addPoints ps
        max = maximum $  map snd $ S.toList pointMap

stepTime :: Int -> (S.Set Point, Point) -> (S.Set Point, Point)
stepTime max a@(set, (x, y)) =
    if y > max 
        then (set, (x,y))
        else 
            case (x, y + 1) `S.member` set of
                False -> (set, (x, y + 1))
                True -> case (x - 1, y + 1) `S.member` set of
                    False -> (set, (x - 1, y + 1))
                    True -> case (x + 1, y + 1) `S.member` set of
                        False -> (set, (x + 1, y + 1))
                        True -> ((x, y) `S.insert` set, (500, 0))

addPoints xs = S.fromList $ do
    ((x1, y1), (x2, y2)) <- pairs
    a <- range x1 x2
    b <- range y1 y2
    pure (a,b)
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







