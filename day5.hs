module Main where
import Common
import qualified Data.Map as M
import Data.Map (Map)
import Data.List
import Data.Bifunctor
import Control.Arrow
import Text.Parsec (runParser)
import Text.Parsec.Char (string, digit)
import Text.Parsec.Combinator (many1)
import Data.Either


main = aoc parse part1 part2

data Move = Move { count :: Int, src :: Int, dst :: Int } deriving (Show)

parse :: String -> (Map Int [Char], [Move])
parse = bimap start (fmap move . lines) . splitOnce "\n\n"
    where
        start = M.fromList . fmap stack . filter ((/= ' ') . head) . fmap reverse . transpose . lines
        stack = (read . (:[]) . head) &&& (reverse . takeWhile (/= ' ') . tail)
        move = fromRight undefined . runParser p () ""
            where
                p = do
                    string "move "
                    count <- read <$> many1 digit
                    string " from "
                    src <- read <$> many1 digit
                    string " to "
                    dst <- read <$> many1 digit
                    return $ Move count src dst

runMove :: (String -> String) -> Map Int String -> Move -> Map Int String
runMove fn state (Move { count, src, dst }) = M.insert src (drop count srcC) $ M.insert dst (fn (take count srcC) ++ dstC) state
    where
        srcC = state M.! src
        dstC = state M.! dst

part1 = fmap snd . sort . fmap (fmap head) .  M.toList . uncurry (foldl (runMove reverse))

part2 = fmap snd . sort . fmap (fmap head) .  M.toList . uncurry (foldl (runMove id))

