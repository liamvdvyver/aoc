module Main where
import Data.Maybe
import qualified Data.Map as Map

-- PART ONE {{{

parseLns :: [String] ->  (String, [(String, (String, String))])
parseLns lines = (lines !! 0, map parseLn $ drop 2 lines)
    where
        parseLn :: String -> (String, (String, String))
        parseLn ln = (wrds !! 0, (init $ tail $ wrds !! 2, init $ wrds !! 3))
            where wrds = words ln


followMap :: (String, [(String, (String, String))]) -> Int -> String -> String -> Int
followMap (instructions, mapList) iterno current destination
    | current == destination = iterno
    | otherwise              = followMap (instructions, mapList) (iterno + 1) next destination
    where
        mapTuple = fromJust $ Map.lookup current (Map.fromList mapList)
        direction = (cycle instructions) !! iterno
        next = case direction of
            'L' -> fst mapTuple
            'R' -> snd mapTuple


solvePartOne :: [String] -> Int
solvePartOne lines = followMap (parseLns lines) 0 "AAA" "ZZZ"

-- }}}

-- NAIVE PART TWO {{{

followMapSimul :: (String, [(String, (String, String))]) -> Int -> [String] -> Int
followMapSimul (instructions, mapList) iterno currents
    | all ((== 'Z') . last) currents = iterno
    | otherwise = followMapSimul (instructions, mapList) (iterno + 1) nexts
    where
        mapTuples = map (fromJust . flip Map.lookup (Map.fromList mapList)) currents
        direction = (cycle instructions) !! iterno
        nexts = case direction of
            'L' -> map fst mapTuples
            'R' -> map snd mapTuples

solvePartTwoNaive :: [String] -> Int
solvePartTwoNaive lines = followMapSimul (instructions, mapList) 0 start
    where
        (instructions, mapList) = parseLns lines
        start = [ x | x <- (map fst mapList), last x == 'A' ]

-- }}}

-- SMARTER PART TWO {{{

-- Given a starting node and move number, find loop length and occurence of Z
-- nodes in loop
findLoop :: (String, [(String, (String, String))]) -> (String, Int) -> String -> Int -> [(String, Int)] -> (Int, [(String, Int)])
findLoop (instructions, mapList) (start, startno) current iterno ret
    | iterno > 0 && current == start && (iterno - startno) `mod` (length instructions) == 0 = (iterno, ret)
    | otherwise = findLoop (instructions, mapList) (start, startno) next (iterno + 1) retNew
    where

        -- find next node
        nextTuple = fromJust $ Map.lookup current (Map.fromList mapList)
        direction = (cycle instructions) !! (iterno + startno)
        next = case direction of
            'L' -> fst nextTuple
            'R' -> snd nextTuple

        -- add current Z node to ret
        retCur = [(current, iterno)]
        retNew = if last current == 'Z' then ret ++ retCur else ret


-- To get the full information we need to solve, find which Z node each A node
-- is mapped to
findZFromA :: (String, [(String, (String, String))]) -> String -> String -> Int -> (String, Int)
findZFromA (instructions, mapList) start current iterno
    | last current == 'Z' = (current, iterno)
    | otherwise = findZFromA (instructions, mapList) start next (iterno + 1)
    where
        -- find next node
        nextTuple = fromJust $ Map.lookup current (Map.fromList mapList)
        direction = (cycle instructions) !! iterno
        next = case direction of
            'L' -> fst nextTuple
            'R' -> snd nextTuple

-- Get both the first Z node and length and occurence of Z nodes in loop from
-- that point
findPath :: (String, [(String, (String, String))]) -> String -> ((String, Int), (Int, [(String, Int)]))
findPath parsed start = (zPath, loops)
    where
        zPath = findZFromA parsed start start 0
        loops = findLoop parsed (firstZ, zIt) firstZ 0 []
        (firstZ, zIt) = zPath

allAs :: (String, [(String, (String, String))]) -> [String]
allAs (instructions, mapList) = filter ((== 'A') . last) (map fst mapList)

getAllPaths :: (String, [(String, (String, String))]) -> [(String, ((String, Int), (Int, [(String, Int)])))]
getAllPaths (instructions, mapList) = map findPath' (allAs (instructions, mapList))
    where
        findPath' :: String -> (String, ((String, Int), (Int, [(String, Int)])))
        findPath' start = (start, findPath (instructions, mapList) start)

-- Note, it looks like loops repeat in the same time as it takes to get to the
-- first Z, so we can just take the LCM of the time taken for all A's to reach a
-- Z node (although this is not specified in the problem)

solvePartTwo :: [String] -> Int
solvePartTwo lines = lcm' pathLengths
    where
        pathLengths = map (snd . (\x -> findZFromA parsed x x 0)) as
        as = allAs parsed
        parsed = parseLns lines
        lcm' :: [Int] -> Int
        lcm' nums = foldl lcm 1 nums

-- }}}

main :: IO ()
main = do
    contents <- readFile "inputs/08.txt"
    let file_lines = lines contents
    print $ parseLns file_lines
    print $ solvePartOne file_lines
    print $ solvePartTwo file_lines
