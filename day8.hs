module Main where
import Common
import Control.Arrow
import Data.List
import Control.Monad

main = aoc parse part1 part2

parse = lines

routes :: [String] -> (Int, Int) -> [String]
routes table (x, y) = lines table x y ++ lines (transpose table) y x
    where
        lines table x y = a $ first reverse $ splitAt x (table !! y)
        a (x, y:ys) = [y:x, y:ys]

part1 table = length . filter id $ any (liftM2 (>) head (maximum . ('/':) . tail)) . routes table <$> ((,) <$> [0..length (head table) - 1] <*> [0..length table - 1])

score table point = product $ dist <$> routes table point
    where
        dist (root:rest) = length $ takeUntil (< root) rest
part2 table = maximum $ score table <$> ((,) <$> [0..length (head table) - 1] <*> [0..length table - 1])

