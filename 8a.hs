{-# LANGUAGE NoImplicitPrelude #-}

import AOC
import qualified Data.Vector as V
import qualified Data.Set as S

main :: IO ()
main = interact $ g . parseGrid ((read :: String -> Int)) 

g = length . filter (==True) . f

or2 :: [Bool] -> [Bool] -> [Bool]
or2 = zipWith (||)

f :: (Show a, Ord a) => [[a]] -> [Bool]
f grid = (join (vtol2 visibleHorizontal)) `or2` (join (transpose $ vtol2 visibleVertical))
    where
        visibleHorizontal = findVisible $ ltov2 grid
        visibleVertical = findVisible $ ltov2 $ transpose grid
    
findVisible grid = do
    row <- grid
    pure $ V.imap (isVisible row) (row)
    where
        isVisible row idx int =
            if (idx == 0 || idx == length row - 1) 
                then True 
                else (int > V.maximum left || int > V.maximum right)
            where 
                left = V.take idx row
                right = V.drop (idx + 1) row