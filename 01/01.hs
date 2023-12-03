module Main where
import System.IO
import Control.Monad

firstNum, lastNum :: String -> Char

firstNum line = firstNum' line 0
    where
        firstNum' :: String -> Int -> Char
        firstNum' line pos
            | [line!!pos] `elem` (map show [0..9]) = line!!pos
            | pos < length line                    = firstNum' line (pos + 1)

lastNum line = lastNum' line (length line - 1)
    where
        lastNum' :: String -> Int -> Char
        lastNum' line pos
            | [line!!pos] `elem` (map show [0..9]) = line!!pos
            | pos >= 0                             = lastNum' line (pos - 1)

lineNum :: String -> Int
lineNum line = read ((firstNum line) : (lastNum line) : []) :: Int

numsSum :: [Int] -> Int
numsSum xs = numsSum' xs
    where
        numsSum' :: [Int] -> Int
        numsSum' [] = 0
        numsSum' (x:xs) = x + numsSum' xs

spelledNums = [
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
isSpelledNum string = string `elem` map fst spelledNums

getSpelledNumTuple :: String -> (String, String)
getSpelledNumTuple string
    | isSpelledNum string = getSpelledNum' string
    | length string == 1 = (string, "_")
    | otherwise = getSpelledNumTuple (map (string!!) [0..(length string - 2)])
    where
        min_num_length' = 3
        getSpelledNum' :: String -> (String, String)
        getSpelledNum' string = (filter (\x -> fst x == string) spelledNums)!!0

-- quadratic time, could be better
spellOutLine :: String -> Int -> String
spellOutLine line pos
    | pos == length line = line
    | otherwise = do
        let leftPart  = if pos == 0 then "" else (map (line!!) [0..(pos - 1)])
        let rightPart = (map (line!!) [pos..(length line - 1)])
        let spelledNumTuple = getSpelledNumTuple rightPart
        let rightPartTrunc = map (rightPart!!) [1..(length rightPart - 1)]
        let lineNew = leftPart ++ (snd spelledNumTuple) ++ rightPartTrunc
        let posNew = pos + 1
        spellOutLine lineNew posNew

solvePartOne :: [String] -> Int
solvePartOne lines = numsSum $ map lineNum lines

solvePartTwo :: [String] -> Int
solvePartTwo lines = numsSum $ map lineNum $ map (\x -> spellOutLine x 0) lines

main :: IO ()
main = do
    file_contents <- readFile "01/input.txt"
    let file_lines = lines file_contents
    print $ solvePartOne file_lines
    print $ solvePartTwo file_lines
