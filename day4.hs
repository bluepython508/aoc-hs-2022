module Main where
import Common
import Data.List
import Control.Monad

main = aoc parse part1 part2

parse :: String -> [[[Integer]]]
parse = fmap (fmap (uncurry enumFromTo . toPair . fmap read . splitOn "-") . splitOn ",") . lines

part1 = length . filter (ap (flip elem) (sort . foldr1 union))


part2 = length . filter (not . null . foldr1 intersect)

