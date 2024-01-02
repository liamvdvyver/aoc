module Main where
import Data.List
import Data.Maybe


parseLns :: [String] -> [[String]] -> [[String]]
parseLns [] ret = ret
parseLns input ret = parseLns newInput newRet
    where
        newInput = drop 1 $ dropWhile (/= "") input
        newRet = ret ++ [takeWhile (/= "") input]


findReflection :: [String] -> Int
findReflection pattern
    | null lineNo = 0
    | otherwise = head lineNo
    where
        firstPart x = reverse $ take x pattern
        secondPart x = drop x pattern
        lineNo = [
            x |
            x <- [1..(length pattern - 1)],
            all (id) $ zipWith (==) (firstPart x) (secondPart x)
            ]


solvePartOne :: [String] -> Int
solvePartOne input = sum $ map scorePattern $ parseLns input []
    where
        scorePattern :: [String] -> Int
        scorePattern pattern =
            100 * (findReflection pattern) +
                (findReflection $ transpose pattern)


offByOne :: (Eq a) => [a] -> [a] -> Bool
offByOne x y = [True] == (filter (id) $ zipWith (/=) x y)


findDiff :: (Eq a) => [a] -> [a] -> Int
findDiff x y = fromJust $ findIndex (not) $ zipWith (==) x y


findPseudoReflection :: [String] -> Int
findPseudoReflection pattern
    | null lineNo = 0
    | otherwise = head lineNo
    where
        firstPart x = reverse $ take x pattern
        secondPart x = drop x pattern
        lineNo = [
            x |
            x <- [1..(length pattern - 1)],
            offByOne (firstPart x) (secondPart x),
            let ind = findDiff (firstPart x) (secondPart x) in
            offByOne (firstPart x !! ind) (secondPart x !! ind)
            ]


solvePartTwo :: [String] -> Int
solvePartTwo input = sum $ map scorePattern' $ parseLns input []
    where
        scorePattern' :: [String] -> Int
        scorePattern' pattern =
            100 * (findPseudoReflection pattern) +
                (findPseudoReflection $ transpose pattern)


main = do
    contents <- readFile "13/input.txt"
    let file_lines = lines contents
    print $ solvePartOne file_lines
    print $ solvePartTwo file_lines
