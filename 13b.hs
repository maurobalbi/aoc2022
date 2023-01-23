{-# LANGUAGE NoImplicitPrelude #-}

import AOC

main :: IO ()
main = interactg $ f . parsePackets

f packets = product $ map (+1) $ findIndices (\a -> a == divisor1 || a == divisor2) $ sort $ [divisor1, divisor2] ++ packets
    where
        divisor1 = Nested [Nested [Literal 2]]
        divisor2 = Nested [Nested [Literal 6]]
data Packet = Nested [Packet] | Literal Int deriving (Eq, Show)

instance Ord Packet 
    where
        p1 `compare` p2 | p1 `rightOrder` p2 = LT
                        | p2 `rightOrder` p1 = GT
                        | otherwise = EQ

rightOrder :: Packet -> Packet -> Bool
Literal i1 `rightOrder` Literal i2 = i1 < i2
Literal i1 `rightOrder` p2 = Nested [Literal i1] `rightOrder` p2
Nested (a:_) `rightOrder` Nested [] = False
p1 `rightOrder` Literal i2 = p1 `rightOrder` Nested [Literal i2]
Nested (a1:rest1) `rightOrder` Nested (a2:rest2) = (a1 == a2 && Nested rest1 `rightOrder` Nested rest2) || a1 `rightOrder` a2
Nested [] `rightOrder` Nested (a:_) = True
_ `rightOrder` _ = False

parsePackets :: [[String]] -> [Packet]
parsePackets = join . map parsePair
    where 
        parsePair [a, b] = [parse' parsePacket a, parse' parsePacket b]

brackets :: Parser a -> Parser a
brackets  = between (char '[') (char ']')

parsePacket :: Parser Packet
parsePacket = Nested <$> brackets (sepBy parsePacket (char ',')) <|> ( Literal <$> integer )
