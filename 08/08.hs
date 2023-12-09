module Main where
import Data.Maybe
import qualified Data.Map as Map


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

solvePartTwo :: [String] -> Int
solvePartTwo lines = followMapSimul (instructions, mapList) 0 start
    where
        (instructions, mapList) = parseLns lines
        start = [ x | x <- (map fst mapList), last x == 'A' ]

main :: IO ()
main = do
    contents <- readFile "08/input_partial.txt"
    let file_lines = lines contents
    print $ parseLns file_lines
    print $ solvePartTwo  file_lines -- 6
