module Main where
import Common
import Data.Either
import Text.Parsec.Char
import Control.Applicative ( Alternative((<|>)) )
import Data.Bifunctor
import Text.Parsec.Prim hiding ((<|>), parse)
import Text.Parsec.Combinator (manyTill, sepBy1, sepEndBy1)
import Text.Parsec (many1, runParser, eof)


main = aoc parse part1 part2

data File = Directory String [File] | File String Integer deriving (Show)

name :: File -> String
name (File name _) = name
name (Directory name _) = name

size :: File -> Integer
size (File _ size) = size
size (Directory _ children) = sum $ size <$> children

isDirectory :: File -> Bool
isDirectory (Directory _ _) = True
isDirectory (File _ _) = False

traverseFileTree :: File -> [File]
traverseFileTree f@(File _ _) = [f]
traverseFileTree f@(Directory _ children) = f : (children >>= traverseFileTree)

modify :: (File -> File) -> [String] -> File -> File
modify fn [final] file
    | name file == final = fn file
modify fn (next:rest) (Directory name children)
    | name == next = Directory name $ fmap (modify fn rest) children
modify _ _ file = file 

parse = fromRight undefined . runParser parser ([], Directory "/" []) ""
    where
        parser = do
            many command
            eof
            snd <$> getState
        
        command = do
            string "$ "
            cd <|> ls
        
        cd = do
            string "cd "
            path <- restLine <* newline
            case path of
                "/" -> modifyState $ first (const ["/"])
                ".." -> modifyState $ first tail
                x -> modifyState $ first (x:)
        
        ls = do
            string "ls\n"
            children <- sepEndBy1 (
                        (flip Directory [] <$> (string "dir " *> restLine))
                    <|> (flip File <$> (read <$> many1 digit <* spaces) <*> restLine)
                ) newline
            modifyState $ \(path, root) -> (path, modify (const $ Directory (head path) children) (reverse path) root)
        
        restLine = manyTill anyChar (lookAhead newline)

part1 = sum . filter (< 100000) . fmap size . filter isDirectory . traverseFileTree

part2 f = minimum . filter (> size f - 40000000) . fmap size . filter isDirectory . traverseFileTree $ f
