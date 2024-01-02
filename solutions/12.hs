module Main where
import Data.List

-- Parsing {{{

parseLn :: String -> (String, [Int])
parseLn line = (string, nums)
    where
        (string:rawNums:[]) = words line
        nums = parseNums rawNums []


parseNums :: String -> [String] -> [Int]
parseNums [] ret = map (\x -> read x :: Int) ret
parseNums str ret = parseNums newStr newRet
    where
        newStr = dropWhile (==',') $ dropWhile (/=',') $ dropWhile (==',') str
        newRet = ret ++ [takeWhile (/=',') $ dropWhile (==',') str]


-- }}}

-- Part one: Brute force {{{

allCombinations :: [String] -> [String]
allCombinations combinations
    | (notElem '?' $ head combinations) = combinations
    | otherwise = allCombinations $ concat $ map (genCombinations) combinations
    where
        genCombinations :: String -> [String]
        genCombinations combinations = [
            firstPart ++ "." ++ lastPart, firstPart ++ "#" ++ lastPart
            ]
            where
                (firstPart, wholeLastPart) = break (=='?') combinations
                lastPart = tail wholeLastPart

strToNums :: String -> [Int]
strToNums str = map length $ filter ((=='#') . head) $ group str

validCombinations :: (String, [Int]) -> [String]
validCombinations (str, nums) = filter ((== nums) . strToNums) $ allCombinations [str]

solvePartOne :: [String] -> Int
solvePartOne lns = length $ concat $ map validCombinations $ map parseLn lns

-- }}}

-- {{{ Part two: DP

splitSequences :: String -> [String]
splitSequences str = start:connector:middle:end:[]
    where
        chars = "?#"
        end = dropWhile (`elem` chars) str
        start = reverse $ dropWhile (`elem` chars) $ reverse str
        middle = dropWhile (`elem` chars) start
        connector = droppedEnd ++ "?" ++ droppedStart
        droppedEnd = reverse $ takeWhile (`elem` chars) $ reverse str
        droppedStart = takeWhile (`elem` chars) str

solveLine :: (String, [Int]) -> Int
solveLine (str, nums) = length upToEnd
    where

        -- Get new target
        newNums :: [Int]
        newNums = concat $ take 5 $ repeat nums

        -- Get all combinations for each subsequence
        (start:connector:middle:end:_) = splitSequences str

        -- Filter anything which produces groups of undesired size
        startCombinations     = filter (all (`elem` nums)) $ map strToNums $ allCombinations [start]
        connectorCombinations = filter (all (`elem` nums)) $ map strToNums $ allCombinations [connector]
        middleCombinations    = filter (all (`elem` nums)) $ map strToNums $ allCombinations [middle]
        endCombinations       = filter (all (`elem` nums)) $ map strToNums $ allCombinations [end]

        -- Combine, filtering those options which do not
        upToStart = [x      | x <- startCombinations,                all id $ zipWith (==) x newNums]
        upToCon1 =  [x ++ y | x <- upToStart, y <- connectorCombinations, all id $ zipWith (==) (x ++ y) newNums]
        upToMid1 =  [x ++ y | x <- upToCon1,  y <- middleCombinations,    all id $ zipWith (==) (x ++ y) newNums]
        upToCon2 =  [x ++ y | x <- upToMid1,  y <- connectorCombinations, all id $ zipWith (==) (x ++ y) newNums]
        upToMid2 =  [x ++ y | x <- upToCon2,  y <- middleCombinations,    all id $ zipWith (==) (x ++ y) newNums]
        upToCon3 =  [x ++ y | x <- upToMid2,  y <- connectorCombinations, all id $ zipWith (==) (x ++ y) newNums]
        upToMid3 =  [x ++ y | x <- upToCon3,  y <- middleCombinations,    all id $ zipWith (==) (x ++ y) newNums]
        upToCon4 =  [x ++ y | x <- upToMid3,  y <- connectorCombinations, all id $ zipWith (==) (x ++ y) newNums]
        upToEnd  =  [x ++ y | x <- upToCon4,  y <- endCombinations,       (x ++ y) == newNums]

-- }}}

main = do
    contents <- readFile "inputs/12.txt"
    let file_lines = lines contents
    print $ solvePartOne file_lines
