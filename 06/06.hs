module Main where
import System.IO
import Data.List
import Control.Monad

parse :: [String] -> [(Integer, Integer)]
parse lines = map (\(t, d) -> (int t, int d)) $ tail $ zip times distances
    where
        (times:(distances:xs)) = map words lines
        int :: String -> Integer
        int str = read str :: Integer

findBounds :: (Integer, Integer) -> (Integer, Integer)
findBounds (t, d) = (minBound, maxBound)
    where
        (t', d') = (fromIntegral t, fromIntegral d)
        (minSol, maxSol) = (
                ((t' - sqrt (t' ** 2 - 4 * d')) / 2),
                ((t' + sqrt (t' ** 2 - 4 * d')) / 2)
            )
        (minBound, maxBound) = (floor $ minSol + 1, ceiling $ maxSol - 1)

solvePartOne :: [String] -> Integer
solvePartOne lines = product $ map (\(lb, ub) -> ub - lb + 1) bounds
    where
        races = parse lines
        bounds = map findBounds races

unkern :: [(Integer, Integer)] -> (Integer, Integer)
unkern [] = (0, 0)
unkern ((t, d):xs) = let (t', d') = unkern xs in (unkern' t t', unkern' d d')
    where
        unkern' :: Integer -> Integer -> Integer
        unkern' a 0 = a -- Bit of a hack, the edge case of a valid 0 not handled
        unkern' a b = read (show a ++ show b) :: Integer

solvePartTwo :: [String] -> Integer
solvePartTwo lines = product $ map (\(lb, ub) -> ub - lb + 1) bounds
    where
        races = (unkern $ parse lines) : []
        bounds = map findBounds races

main = do
    file_contents <- readFile "06/input.txt"
    let file_lines = lines file_contents
    print $ parse file_lines
    print $ unkern $ parse file_lines
    print $ solvePartOne file_lines
    print $ solvePartTwo file_lines
