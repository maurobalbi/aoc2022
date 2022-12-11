{-# LANGUAGE NoImplicitPrelude #-}

import AOC

main :: IO ()
main = interact $ f . parseList p

data ElfRPS = A | B | C deriving (Show, Eq, Read, Ord, Bounded, Enum)

data MyRPS = X | Y | Z deriving (Show, Eq, Read, Ord, Bounded, Enum)

p :: Parser (ElfRPS, MyRPS)
p = do
  e <- enump
  char ' '
  m <- enump
  pure (e, m)

f :: Foldable t => t (ElfRPS, MyRPS) -> Int
f xs = foldr ((+) . calculatePoints ) 0 xs

calculatePoints :: (ElfRPS, MyRPS) -> Int
calculatePoints (A, X) = 1 + 3
calculatePoints (A, Y) = 2 + 6
calculatePoints (A, Z) = 3 + 0
calculatePoints (B, X) = 1 + 0
calculatePoints (B, Y) = 2 + 3
calculatePoints (B, Z) = 3 + 6
calculatePoints (C, X) = 1 + 6
calculatePoints (C, Y) = 2 + 0
calculatePoints (C, Z) = 3 + 3
