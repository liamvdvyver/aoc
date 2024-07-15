module Main where
import System.IO
import Control.Monad
import SplitOn

--- PART ONE {{{

-- Parse individual lines in file
parseSeeds :: String -> [Int]
parseSeeds line = map (\x -> read x :: Int) $ tail $ splitOn " " line

parseHeader :: String -> String
parseHeader line = (splitOn " " line) !! 0

parseData :: String -> (Int, Int, Int)
parseData line = (splitted !! 0, splitted !! 1, splitted !! 2)
    where
        splitted :: [Int]
        splitted = map (\x -> read x :: Int) $ splitOn " " line

-- Parses block for map to single map tuple
parseMap :: [String] -> (String, [(Int, Int, Int)])
parseMap (x:xs) = (parseHeader x, map parseData xs)

-- Turns file lines into blocks
linesToBlocks :: [String] -> [[String]]
linesToBlocks lines = linesToBlocks' lines 0 []
    where
        linesToBlocks' :: [String] -> Int -> [[String]] -> [[String]]
        linesToBlocks' lines pos ret
            | pos >= length lines = ret ++ [lines]
            | lines !! pos == ""  = linesToBlocks' rightPart 0 (ret ++ leftPart)
            | otherwise = linesToBlocks' lines (pos + 1) ret
                where
                    leftPart = [(map (lines !!) [0..(pos - 1)])]
                    rightPart = (map (lines !!) [(pos + 1)..(length lines - 1)])

-- Combines these helpers, gets you a tuple of seed list and list of map tuples
parseFile :: [String] -> ([Int], [(String, [(Int, Int, Int)])])
parseFile lines = (seeds, maps)
    where
        seeds = parseSeeds $ head $ head $ linesToBlocks lines
        maps = map parseMap $ tail $ linesToBlocks lines

-- Source and single map to destination
lookupMap :: [(Int, Int, Int)] -> Int -> Int
lookupMap [] source = source
lookupMap ((destStart, sourceStart, rangeLength):xs) source
    | offset < rangeLength && offset >= 0 = destStart + offset
    | otherwise = lookupMap xs source
        where offset = source - sourceStart

-- Source and set of maps (as list of map tuples) to destination
lookupMapSequence :: [(String, [(Int, Int, Int)])] -> Int -> Int
lookupMapSequence [] source = source
lookupMapSequence ((_, map):xs) source = lookupMapSequence xs dest
    where dest = lookupMap map source

-- Here, just brute force it
solvePartOne :: [String] -> Int
solvePartOne lines = foldl min maxLocation locations
    where
        (seeds, maps) = parseFile lines
        locations = map (lookupMapSequence maps) seeds
        maxLocation = foldl max 0 locations

-- }}}

-- PART TWO {{{

-- Convert list of seeds to longer list of all seeds, for brute force approach
-- Ultimately, traversing the list each iteration to check membership is too
-- slow
seedsToRanges :: [Int] -> [Int]
seedsToRanges seeds = seedsToRanges' seeds []
    where
        seedsToRanges' :: [Int] -> [Int] -> [Int]
        seedsToRanges' [] ret = ret
        seedsToRanges' (start:(range:xs)) ret = seedsToRanges' xs retNew
            where
                retNew = ret ++ [start..(start + range)]

-- So we can find which seed is planted in a given location
reverseMaps :: [(String, [(Int, Int, Int)])] -> [(String, [(Int, Int, Int)])]
reverseMaps mapList = reverse swappedList
    where
        swappedList = map swapSnd mapList
        swapSnd :: (String, [(Int, Int, Int)]) -> (String, [(Int, Int, Int)])
        swapSnd (name, mapRanges) = (name, map reverseDestSource mapRanges)
        reverseDestSource :: (Int, Int, Int) -> (Int, Int, Int)
        reverseDestSource (a, b, c) = (b, a, c)

-- This checks ownership of an unexpanded range much faster
inRange :: [Int] -> Int -> Bool
inRange [] seed = False
inRange (start:(range:xs)) seed
    | offset < range && offset >= 0 = True
    | otherwise = inRange xs seed
        where offset = seed - start

-- Given a reversed map list, target seeds, and a current destination
-- Iterate upwards to find the lower destination number which a seed in the
-- range will be planted in
reverseFindLocation :: [(String, [(Int, Int, Int)])] -> [Int] -> Int -> Int
reverseFindLocation reverseMapList seedsRaw dest
    | inSeedRanges reverseMapList seedsRaw dest = dest
    | dest == iterLimit = 0
    | otherwise = reverseFindLocation reverseMapList seedsRaw (dest + 1)
        where
            iterLimit = 999999999
            inSeedRanges :: [(String, [(Int, Int, Int)])] -> [Int] -> Int -> Bool
            inSeedRanges reverseMapList seedsRaw dest = inRange seedsRaw reversed
            reversed = lookupMapSequence reverseMapList dest

-- Use this method to find the lowest location, given a raw range
lowestLocation :: [(String, [(Int, Int, Int)])] -> [Int] -> Int
lowestLocation mapList seedsRaw = reverseFindLocation reverseMapList seedsRaw 0
    where
        reverseMapList = reverseMaps mapList

solvePartTwo :: [String] -> Int
solvePartTwo lines = lowestLocation maps seedsRaw
    where
        (seedsRaw, maps) = parseFile lines

--- }}}

main :: IO ()
main = do
    file_contents <- readFile "inputs/05.txt"
    let file_lines = lines file_contents
    print $ solvePartOne file_lines
    print $ solvePartTwo file_lines
