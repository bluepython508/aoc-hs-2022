module Main where
import Common
import Data.List
import Control.Monad
import Data.Maybe

main = aoc parse part1 part2

parse s n = (n+) . fromJust . findIndex (ap ((==) . nub) id . sort) . slidingWindow n $ s

part1 :: (Int -> Int) -> Int
part1 = ($ 4)

part2 = ($ 14)