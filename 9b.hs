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
f directions = length $ S.toList $ execState (advance directions) S.empty

advance :: [Direction] -> State (S.Set Point) [Point]
advance = advance' (take 10 $ repeat (0,0))
    where
        advance' knots (front:rest) = do 
            modify (S.insert (last knots)) 
            advance' (moveAndFollow front knots) rest
        advance' knots [] = do
            modify (S.insert (last knots))
            pure knots

moveAndFollow :: Direction -> [Point] -> [Point]
moveAndFollow direction (head: knots) = reverse $ foldl' (\kn@(fol:acc) p -> p + (follow (p - fol)):kn) [newLocation] knots
    where 
        newLocation = head + (dir direction)

follow :: Point -> Point
follow (-2, 2) = (1, -1)
follow (-2, -2) = (1, 1)
follow (2, -2) = (-1, 1)
follow (2,2) = (-1, -1)
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

