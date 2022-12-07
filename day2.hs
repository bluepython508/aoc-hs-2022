module Main where
import Common
import Data.List (find)
import Data.Maybe
import Control.Arrow

main = aoc parse part1 part2

data Move = Rock | Paper | Scissors deriving (Eq, Show)
data Outcome = Win | Draw | Loss deriving (Eq, Show)

parse :: String -> [(Move, Char)]
parse = fmap (move . words) . lines
    where
        move [opp, [cur]] = (
            case opp of
                "A" -> Rock
                "B" -> Paper
                "C" -> Scissors,
            cur
            )

forOutcome Rock Win = Paper
forOutcome Paper Win = Scissors
forOutcome Scissors Win = Rock
forOutcome x Draw = x
forOutcome x Loss = forOutcome (forOutcome x Win) Win

score :: Move -> Move -> Integer
score opp cur = scoreOutcome outcome + scoreShape cur
    where
        outcome = fromJust $ find ((== cur) . forOutcome opp) [Win, Draw, Loss]

        scoreOutcome Win = 6
        scoreOutcome Draw = 3
        scoreOutcome Loss = 0

        scoreShape Rock = 1
        scoreShape Paper = 2
        scoreShape Scissors = 3

part1 = sum . fmap (uncurry score . fmap move)
    where
        move 'X' = Rock
        move 'Y' = Paper
        move 'Z' = Scissors

part2 = sum . fmap (uncurry score . (fst &&& uncurry forOutcome) . fmap outcome)
    where
        outcome 'X' = Loss
        outcome 'Y' = Draw
        outcome 'Z' = Win