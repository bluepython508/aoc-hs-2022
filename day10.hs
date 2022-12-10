module Main where
import Common
import Control.Monad
import Control.Arrow
import Data.List

main = aoc parse part1 part2

parse = lines >=> i
    where
        i "noop" = [0]
        i ('a':'d':'d':'x':' ':rest) = [0, read rest] 

part1 xs = show $ sum $ uncurry (*) . (id &&& ((scanl (+) 1 xs !!) . subtract 1)) <$> [20, 60, 100, 140, 180, 220]
part2 xs = intercalate "\n" $ chunks 40 $ fmap s $ zipWith lit [0..] $ scanl (+) 1 xs
    where
        lit px val = abs (px `mod` 40 - val) <= 1
        s True = '#'
        s False = ' '

