{-# LANGUAGE NoImplicitPrelude #-}

import AOC
import qualified Data.Vector as V
import qualified Data.Set as S

main :: IO ()
main = interact $ maximum . f . parseGrid ((read :: String -> Int)) 

times :: [Int] -> [Int] -> [Int]
times = zipWith (*)

f :: (Show a, Ord a) => [[a]] -> [Int]
f grid = (join (vtol2 visibleHorizontal)) `times` (join (transpose $ vtol2 visibleVertical))
    where
        visibleHorizontal = findVisible $ ltov2 grid
        visibleVertical = findVisible $ ltov2 $ transpose grid
    
findVisible grid = do
    row <- grid
    pure $ V.imap (amountVisible row) (row)
    where
        amountVisible row idx int =
            if (idx == 0 || idx == length row - 1) 
                then 0
                else (min (length left) (1 + (length $ V.takeWhile (< int) $ left))) * (min (length right) (1 + (length $ V.takeWhile (< int) right)))
            where 
                left = V.reverse $ V.take idx row
                right = V.drop (idx + 1) row