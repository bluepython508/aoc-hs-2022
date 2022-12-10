{-# LANGUAGE UndecidableInstances #-}
module Common where
import Control.Arrow
import Data.Bifunctor
import Control.Monad
import qualified Data.Text as T

class Printable a where
    toString :: a -> String

instance {-# OVERLAPPING #-} Printable String where
    toString = id

instance Show a => Printable a where
    toString = show

aoc :: Printable b => (String -> a) -> (a -> b) -> (a -> b) -> IO ()
aoc parse part1 part2 = uncurry (*>)
    . bimap (putStr . ("Part 1:\n"++) . (++"\n\n") . toString) (putStrLn . ("Part 2:\n"++) . toString)
    . (part1 &&& part2)
    . parse
    =<< getContents

splitOn :: String -> String -> [String]
splitOn sep = fmap T.unpack . T.splitOn (T.pack sep) . T.pack

splitOnce :: String -> String -> (String, String)
splitOnce = ((.).(.)) toPair splitOn

toPair :: [a] -> (a, a)
toPair [x, y] = (x, y)

pairToList :: (a, a) -> [a]
pairToList (x, y) = [x, y]

slidingWindow :: Int -> [a] -> [[a]]
slidingWindow 1 lst = return <$> lst
slidingWindow n lst = zipWith (:) lst (slidingWindow (n - 1) (drop 1 lst))

chunks :: Int -> [a] -> [[a]]
chunks n [] = []
chunks n lst = take n lst : chunks n (drop n lst)

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil fn [] = []
takeUntil fn (x:xs)
    | fn x = x:takeUntil fn xs
    | otherwise = [x]