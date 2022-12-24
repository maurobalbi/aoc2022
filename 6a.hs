{-# LANGUAGE NoImplicitPrelude #-}

import AOC
import Data.Char
import qualified Data.Vector as V

main :: IO ()
main = interact' run

run :: String -> Int
run str = snd . head .  filter (distinctTuple . fst) $ zip (zip4 str (drop 1 str) (drop 2 str) (drop 3 str)) [4..]

distinctTuple :: Eq a => (a, a, a, a) -> Bool
distinctTuple (e1, e2, e3, e4) = 4 == (length $ nub [e1, e2, e3, e4])