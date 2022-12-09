module Main where
import Common
import Control.Arrow
import Data.Char (ord)
import Data.List (intersect)

main = aoc parse part1 part2

parse = lines

priority x | x `elem` ['a' .. 'z'] = ord x - ord 'a' + 1
priority x = ord x - ord 'A' + 27

part1 = sum . fmap (priority . head . uncurry intersect . uncurry splitAt . ((flip div 2 . length) &&& id))

part2 = sum . fmap (priority . head . foldl1 intersect) . chunks 3
