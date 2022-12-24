{-# LANGUAGE NoImplicitPrelude #-}

import AOC
import Data.Char

main :: IO ()
main = interact $ f . parseList p

data Range = Range { min:: Int, max::Int }

f :: [(Range, Range)] -> Int
f = length . filter isSubRange

isSubRange :: (Range, Range) -> Bool
isSubRange (Range min1 max1, Range min2 max2) =
  min1 <= min2 && max1 >= max2 || (min2 <= min1 && max2 >= max1)

p :: Parser (Range, Range)
p = do
  p1 <- pair
  char ','
  p2 <- pair
  pure (p1, p2)
    where
      pair = do 
        min <- integer
        char '-'
        max <- integer
        pure (Range min max)
  

