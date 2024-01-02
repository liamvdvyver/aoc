module Main where
import Data.List

parseLns :: [String] -> [[Integer]]
parseLns lines = map ((map (\x -> read x :: Integer) . words)) lines

getDiffs :: [Integer] -> [Integer]
getDiffs nums = zipWith (-) (drop 1 nums) nums

genDiffPyramid :: [[Integer]] -> [[Integer]]
genDiffPyramid pyramid
    | all (== 0) $ last pyramid = init pyramid ++ [last pyramid ++ [0]]
    | otherwise                 =
        let newPyramid = pyramid ++ [getDiffs $ last pyramid] in genDiffPyramid newPyramid

predictNext :: [[Integer]] -> [[Integer]]
predictNext [x] = [x]
predictNext pyramid = predictNext newPyramid
    where
        diff = last $ last pyramid
        truncPyramid = init pyramid
        lastElem = last $ last truncPyramid
        newElem = lastElem + diff
        newPyramid = init truncPyramid ++ [last truncPyramid ++ [newElem]]

solvePartOne :: [String] -> Integer
solvePartOne lines = sum $ map (\x -> last $ head $ predictNext $ genDiffPyramid [x]) $ parseLns lines

predictPrev :: [[Integer]] -> [[Integer]]
predictPrev [x] = [x]
predictPrev pyramid = predictPrev newPyramid
    where
        diff = head $ last pyramid
        truncPyramid = init pyramid
        firstElem = head $ last truncPyramid
        newElem = firstElem - diff
        newPyramid = init truncPyramid ++ [newElem : (last truncPyramid)]

solvePartTwo :: [String] -> Integer
solvePartTwo lines = sum $ map (\x -> head $ head $ predictPrev $ genDiffPyramid [x]) $ parseLns lines

main = do
    contents <- readFile "inputs/09.txt"
    let file_lines = lines contents
    print file_lines
    print $ solvePartOne file_lines
    print $ solvePartTwo file_lines
