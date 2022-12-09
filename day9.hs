module Main where
import Common
import Data.Bifunctor
import Data.List

main = aoc parse part1 part2

data Vector = Vector Integer Integer deriving (Eq, Ord, Show)

vZero :: Vector
vZero = Vector 0 0

vPlus :: Vector -> Vector -> Vector
vPlus (Vector x y) (Vector x' y') = Vector (x + x') (y + y')

parse = concatMap (uncurry (flip replicate . d) . bimap head (read @Int . tail) . splitAt 1) . lines
    where
        d 'U' = Vector   0   1
        d 'D' = Vector   0 (-1)
        d 'L' = Vector (-1)  0
        d 'R' = Vector   1   0

step :: Vector -> [Vector] -> [Vector]
step move points = zipWith vPlus moves points
    where
        moves = move : zipWith3 ((moveFrom .) . vPlus) points moves (drop 1 points)
        moveFrom (Vector hX hY) (Vector tX tY)
            | abs (hX - tX) > 1 || abs (hY - tY) > 1 = Vector (signum $ hX - tX) (signum $ hY - tY)
            | otherwise = vZero

run :: Int -> [Vector] -> Int
run n = length . nub . sort . fmap last . scanl (flip step) (replicate n vZero)

part1 = run 2

part2 = run 10
