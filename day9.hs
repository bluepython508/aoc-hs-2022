module Main where
import Common
import Control.Arrow
import Data.Bifunctor
import Control.Monad.State
import Data.Set (Set)
import qualified Data.Set as S

main = aoc parse part1 part2

data Vector = Vector Integer Integer deriving (Eq, Ord, Show)
instance Num Vector where
  (Vector x y) + (Vector x' y') = Vector (x + x') (y + y')
  (*) = undefined
  abs = undefined
  signum = undefined
  fromInteger = undefined
  negate (Vector x y) = Vector (negate x) (negate y)

vZero :: Vector
vZero = Vector 0 0

parse = concatMap (uncurry (flip replicate . d) . bimap head (read @Int . tail) . splitAt 1) . lines
    where
        d 'U' = Vector   0   1
        d 'D' = Vector   0 (-1)
        d 'L' = Vector (-1)  0
        d 'R' = Vector   1   0

data RState = RState { points :: [Vector], visited :: Set Vector } deriving (Show)
modifyPoints :: ([Vector] -> [Vector]) -> RState -> RState
modifyPoints fn state = state { points = fn (points state) }

visit :: Vector -> RState -> RState
visit v st = st { visited = S.insert v (visited st) }

step :: Vector -> State RState ()
step x = do
    modify $ modifyPoints $ step x
    last <- gets (last . points)
    modify $ visit last
    where
        movesNext (Vector nX nY) (Vector tX tY)
            | abs (nX - tX) > 1 || abs (nY - tY) > 1 = Vector (signum $ nX - tX) (signum $ nY - tY)
            | otherwise = vZero
        step :: Vector -> [Vector] -> [Vector]
        step move [n] = [n + move]
        step move (n:rest) = n + move : step (movesNext (n + move) $ head rest) rest


run :: Int -> [Vector] -> Int
run n instructions = S.size $ visited $ execState (mapM_ step instructions) $ RState { points = replicate n vZero, visited = S.fromList [vZero] }

part1 = run 2

part2 = run 10

