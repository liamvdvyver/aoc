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

main :: IO ()
main = do
    file_contents <- readFile "input.txt"
    let file_lines = lines file_contents
    let nums = map linenum file_lines
    let final_sum = nums_sum nums
    print final_sum
