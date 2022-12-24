{-# LANGUAGE NoImplicitPrelude #-}

import AOC
import Data.Char
import qualified Data.Vector as V

main :: IO ()
main = interact' $ f . parse' parseInput

type Stack = Vector Char

data Move = Move { amount :: Int, from :: Int, to :: Int } deriving (Show)

f :: (Vector Stack, [Move]) -> [Char]
f (initialState, moves) = vtol . V.map V.head $ execState (foldM (\_ -> modifyStack) () moves) initialState

modifyStack :: Move -> State (Vector Stack) ()
modifyStack (Move amount from to) = do
  modify (\state -> updateState state)
  pure ()
    where 
      updateState state = state V.// [(from - 1 , fromStackAfterMove), (to - 1, toStackAfterMove)]
        where
          fromStack = state ! (from - 1)
          toStack = state ! (to - 1)
          packetsToMove = V.take amount $ fromStack
          fromStackAfterMove = V.drop amount $ fromStack
          toStackAfterMove = (packetsToMove) V.++ toStack

parseInput :: Parser (Vector Stack, [Move])
parseInput = do
  stack <- parseStack
  many1 $ do
    char ' '
    (alphaNum <|> (char ' ') <|> endOfLine)
  endOfLine
  move <- sepEndBy1 parseMove endOfLine
  pure (stack, move)

parseMove :: Parser Move
parseMove = try $ do
  string "move "
  amount <- integer
  string " from "
  from <- integer
  string " to "
  to <- integer
  pure $ Move amount from to

parseStack :: Parser (Vector Stack)
parseStack = do
  initialStack <- parseStackLine
  pure $ ltov $ map (ltov . catMaybes) $ transpose initialStack

parseStackLine :: Parser [[Maybe Char]]
parseStackLine = do
  elems <- sepEndBy1 parseStackElem (endOfLine)
  pure $ elems
    where 
      parseStackElem = 
        do
          s <- Just <$> between (char '[') (char ']') alphaNum <|> nothing
          pure s
        `sepBy1` (char ' ')
      nothing = do
        _ <- try $ string "   "
        pure Nothing
