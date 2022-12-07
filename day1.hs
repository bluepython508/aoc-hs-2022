module Main where
import Common

import Data.List

main = aoc parse part1 part2

parse = fmap (fmap (read @Integer) . lines) . splitOn "\n\n"

part1 = maximum . fmap sum

part2 = sum . take 3 . reverse . sort . fmap sum
