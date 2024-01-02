module Main where
import Data.List
import qualified Data.Set as Set


galaxies :: [String] -> Set.Set (Int, Int)
galaxies spaceMap = Set.fromList [(x, y) | x <- [0..xMax], y <- [0..yMax], (spaceMap !! y) !! x == '#' ]
    where (xMax, yMax) = (length (head spaceMap) - 1, length spaceMap - 1)


getGalaxyPairs :: Set.Set (Int, Int) -> [((Int, Int), (Int, Int))]
getGalaxyPairs galaxySet =
    Set.toList $
        Set.filter (\(a, b) -> fst a < fst b || (fst a == fst b && snd a < snd b)) $
            Set.cartesianProduct galaxySet galaxySet


getEmptyRowsCols :: [String] -> ([Int], [Int])
getEmptyRowsCols spaceMap = (emptyRows, emptyCols)
    where
        emptyRows = findIndices (all (=='.')) spaceMap
        emptyCols = findIndices (all (=='.')) $ transpose spaceMap


blankLinesBetween :: ([Int], [Int]) -> (Int, Int) -> (Int, Int) -> (Int, Int)
blankLinesBetween (emptyRows, emptyCols) (x1, y1) (x2, y2) = (nx, ny)
    where
        nx = length $ filter (\x -> (x1 < x && x < x2) || (x2 < x && x < x1)) emptyCols
        ny = length $ filter (\y -> (y1 < y && y < y2) || (y2 < y && y < y1)) emptyRows


getDistance :: ([Int], [Int]) -> Int -> ((Int, Int), (Int, Int)) -> Int
getDistance emptyRowsCols multiplier ((x1, y1), (x2, y2)) = manhattanDist + expanded
    where
        manhattanDist = abs (x1 - x2) + abs (y1 - y2)
        (nx, ny) = blankLinesBetween emptyRowsCols (x1, y1) (x2, y2)
        expanded = (multiplier - 1) * (nx + ny)


solvePartOne :: [String] -> Int
solvePartOne spaceMap = sum $ map (getDistance emptyRowsCols 2) galaxyPairs
    where
        emptyRowsCols = getEmptyRowsCols spaceMap
        galaxySet = galaxies spaceMap
        galaxyPairs = getGalaxyPairs galaxySet


solvePartTwo :: [String] -> Int
solvePartTwo spaceMap = sum $ map (getDistance emptyRowsCols 1000000) galaxyPairs
    where
        emptyRowsCols = getEmptyRowsCols spaceMap
        galaxySet = galaxies spaceMap
        galaxyPairs = getGalaxyPairs galaxySet


main = do
    contents <- readFile "inputs/11.txt"
    let file_lines = lines contents
    print $ solvePartOne file_lines
    print $ solvePartTwo file_lines
