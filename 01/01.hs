module Main where
import System.IO
import Control.Monad

firstnum, lastnum :: String -> Char

firstnum line = firstnum' line 0
    where
        firstnum' :: String -> Int -> Char
        firstnum' line pos
            | [line!!pos] `elem` (map show [0..9]) = line!!pos
            | pos < length line                    = firstnum' line (pos + 1)

lastnum line = lastnum' line (length line - 1)
    where
        lastnum' :: String -> Int -> Char
        lastnum' line pos
            | [line!!pos] `elem` (map show [0..9]) = line!!pos
            | pos >= 0                             = lastnum' line (pos - 1)

linenum :: String -> Int
linenum line = read ((firstnum line) : (lastnum line) : []) :: Int

nums_sum :: [Int] -> Int
nums_sum xs = nums_sum' xs
    where
        nums_sum' :: [Int] -> Int
        nums_sum' [] = 0
        nums_sum' (x:xs) = x + nums_sum' xs

spelled_nums = [
    ("one",   "1"),
    ("two",   "2"),
    ("three", "3"),
    ("four",  "4"),
    ("five",  "5"),
    ("six",   "6"),
    ("seven", "7"),
    ("eight", "8"),
    ("nine",  "9")
    ] ++ (zip (map show [1..9]) (map show [1..9]))

isSpelledNum :: String -> Bool
isSpelledNum string = string `elem` map fst spelled_nums

getSpelledNumTuple :: String -> (String, String)
getSpelledNumTuple string
    | isSpelledNum string = getSpelledNum' string
    | length string == 1 = (string, "_")
    | otherwise = getSpelledNumTuple (map (string!!) [0..(length string - 2)])
    where
        min_num_length' = 3
        getSpelledNum' :: String -> (String, String)
        getSpelledNum' string = (filter (\x -> fst x == string) spelled_nums)!!0

-- quadratic time, could be better
spellOutLine :: String -> Int -> String
spellOutLine line pos
    | pos == length line = line
    | otherwise = do
        let leftPart  = if pos == 0 then "" else (map (line!!) [0..(pos - 1)])
        let rightPart = (map (line!!) [pos..(length line - 1)])
        let spelledNumTuple = getSpelledNumTuple rightPart
        -- let spelledNumReplacementLength = length (snd spelledNumTuple) -- 1
        let rightPartTrunc = map (rightPart!!) [1..(length rightPart - 1)]
        let lineNew = leftPart ++ (snd spelledNumTuple) ++ rightPartTrunc
        let posNew = pos + 1
        spellOutLine lineNew posNew

solvePartOne :: [String] -> Int
solvePartOne lines = nums_sum $ map linenum lines

solvePartTwo :: [String] -> Int
solvePartTwo lines = nums_sum $ map linenum $ map (\x -> spellOutLine x 0) lines

main :: IO ()
main = do
    file_contents <- readFile "01/input.txt"
    let file_lines = lines file_contents
    print $ solvePartOne file_lines
    print $ solvePartTwo file_lines
