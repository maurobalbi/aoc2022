{-# LANGUAGE NoImplicitPrelude #-}

import AOC
import Data.Char
import Debug.Trace

main :: IO ()
main = interact $ (\cmds -> (sum . filter (< 100000) . getDirSizes <$> runCreateFS cmds)) . parseList parseCommand

type Name = String
type Size = Int
data FSItem = File Name Size | Folder Name [FSItem] deriving (Show) 

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)
type FSZipper = (FSItem, [FSCrumb])  

data Command = CdUp | CdTo Name | LS | OutPut FSItem deriving (Show)

data FolderSizes = FolderS Size [FolderSizes] deriving (Show)

getDirSizes :: FSItem -> [Size]
getDirSizes (Folder name items) = getDirSize (Folder name items) : concatMap getDirSizes items
getDirSizes _ = []

getDirSize :: FSItem -> Size
getDirSize (Folder name items) = sum $ map getDirSize items
getDirSize (File name size) = size

runCreateFS :: [Command] -> Maybe FSItem
runCreateFS cmds = getTreeFromZipper <$> (createFSZipper cmds)

createFSZipper :: [Command] -> Maybe FSZipper
createFSZipper cmds = foldl' (flip modifyFS) Nothing cmds

modifyFS :: Command -> Maybe FSZipper -> Maybe FSZipper
modifyFS ((CdTo "/")) _ = pure $ (Folder "/" [], [])
modifyFS (CdTo name) zipper = zipper >>= fsTo name
modifyFS (CdUp) zipper = zipper >>= fsUp
modifyFS (LS) zipper = zipper
modifyFS (OutPut fileOrFolder) zipper = (fsNewFile fileOrFolder <$> zipper)

(-:) :: t1 -> (t1 -> t2) -> t2
x -: f = f x
getTreeFromZipper :: FSZipper -> FSItem
getTreeFromZipper = fst . goToTop

goToTop :: FSZipper -> FSZipper 
goToTop zipper = case fsUp zipper of
    Nothing -> zipper
    Just (zip) -> goToTop zip

fsUp :: FSZipper -> Maybe FSZipper
fsUp (item, []) = Nothing
fsUp (item, FSCrumb name ls rs:bs) = Just (Folder name (ls ++ [item] ++ rs), bs)
  
fsTo :: Name -> FSZipper -> Maybe FSZipper  
fsTo name (Folder folderName items, bs) =
    case break (nameIs name) items 
        of (ls, []) -> Nothing
           (ls, item: rs) -> Just (item, FSCrumb folderName ls rs:bs)
  
nameIs :: Name -> FSItem -> Bool  
nameIs name (Folder folderName _) = name == folderName  
nameIs name (File fileName _) = name == fileName

fsNewFile :: FSItem -> FSZipper -> FSZipper  
fsNewFile item (Folder folderName items, bs) =   
    (Folder folderName (item:items), bs)
fsNewFile item zipper = error $ show zipper ++ show item

parseCommand :: Parser Command
parseCommand = 
    try (do
        string "$ cd .."
        pure $ CdUp)
    <|> 
    try (do 
        string "$ cd "
        name <- many1 alphaNum <|> string "/"
        pure $ CdTo name)
    <|>
    try (do
        string "$ ls"
        pure LS)
    <|>
    do
        size <- integer
        char ' '
        name <- many1 (alphaNum <|> char '.')
        pure (OutPut $ File name size)
    <|>
    do
        string "dir "
        name <- many1 alphaNum
        pure (OutPut $ Folder name [])
