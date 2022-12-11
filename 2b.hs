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
calculatePoints (A, X) = 0 + 3
calculatePoints (A, Y) = 3 + 1
calculatePoints (A, Z) = 6 + 2
calculatePoints (B, X) = 0 + 1
calculatePoints (B, Y) = 3 + 2
calculatePoints (B, Z) = 6 + 3
calculatePoints (C, X) = 0 + 2
calculatePoints (C, Y) = 3 + 3
calculatePoints (C, Z) = 6 + 1
