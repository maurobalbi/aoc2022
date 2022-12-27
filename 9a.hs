{-# LANGUAGE NoImplicitPrelude #-}

import AOC
import qualified Data.Set as S

main :: IO ()
main = interact $ f . parseInput

data Direction = R | L | U | D deriving (Enum, Ord, Eq, Bounded, Show)

type Point = (Int,Int)

origin :: Point
origin = (0,0)

f :: [Direction] -> Int
f directions = length $ execState (advance directions) S.empty

advance :: [Direction] -> State (S.Set Point) (Point, Point)
advance = advance' (origin, origin)
    where
        advance' (head, tail) (front:rest) = do 
            modify (S.insert tail) 
            advance' (moveAndFollow front (head, tail)) rest
        advance' (head, tail) [] = do
            modify (S.insert tail)
            pure (head, tail)

moveAndFollow :: Direction -> (Point, Point) -> (Point, Point)
moveAndFollow direction (head, tail) = (newLocation, tail + (follow (tail - newLocation)))
    where 
        newLocation = head + (dir direction)

follow :: Point -> Point
follow (-2, y) = (1, y * (-1))
follow (2, y) = (-1, y * (-1))
follow (x, -2) = (x * (-1), 1)
follow (x, 2) = (x * (-1), -1)
follow _ = (0,0)

dir :: Direction -> Point
dir R = (1,0)
dir L = (-1, 0)
dir U = (0,1)
dir D = (0,-1)

parseInput :: [String] -> [Direction]
parseInput = join . parseList p

p :: Parser [Direction]
p = do 
        dir <- enump
        char ' '
        amount <- integer
        pure $ replicate amount dir
