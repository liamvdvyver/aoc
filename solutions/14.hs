module Main where
import Data.List

splitBoulder :: String -> [String] -> [String]
splitBoulder [] ret = ret
splitBoulder str ret = splitBoulder newStr newRet
    where
        first = head str
        newStr = if first == '#' then drop 1 str else dropWhile (/='#') str
        newRet = if first == '#' then ret ++ ["#"] else ret ++ [takeWhile (/='#') str]

rollToLeft :: String -> String
rollToLeft str = concat $ map (reverse . sort) $ splitBoulder str []

-- Asume rolled to left
getLoad :: String -> Int
getLoad str = sum $ map snd $ filter (\x -> fst x == 'O') $ zip str [(length str), (length str - 1)..1]

solvePartOne :: [String] -> Int
solvePartOne lns = sum $ map (getLoad . rollToLeft) $ transpose lns

-- Note, this is deterministic
spinCycle :: [String] -> [String]
spinCycle lns = shiftRotate $ shiftRotate $ shiftRotate $ shiftRotate lns
    where
        rotate strs = (reverse . transpose) strs
        shiftRotate strs = (rotate . map rollToLeft) strs

nSpins :: Int -> Int -> [[String]] -> [String] -> [String]
nSpins max count history state
    | count == max = state
    -- We need to check for loops, if the last few items in history are the same
    | spun == state = state
    -- If there's a loop, increment count, and clear history so next iteration
    -- will proceed as normal to completion
    | loopLength < itersLeft && loopLength > 0 = nSpins max countAfterLooping [] state
    | otherwise = nSpins max (count + 1) (history ++ [spun]) spun
    where
        spun = spinCycle state
        loopLength = length $ findLoops history
        itersLeft = max - count
        countAfterLooping = max - (itersLeft `mod` loopLength)
    
-- If the element was in history, the part between occurrences will loop
-- Return this part
findLoops :: (Eq a) => [a] -> [a]
findLoops [] = []
findLoops a
    | last a `notElem` tail a = []
    | last a `elem` tail a = tail sndPart
    where
        recentLoop = tail $ sndPart
        (fstPart, sndPart) = break (== last a ) a

solvePartTwo :: [String] -> Int
solvePartTwo lns = sum $ map getLoad $ nSpins 1000000000 0 [tLines] tLines
    where
        tLines = transpose lns

main = do
    contents <- readFile "14/input.txt"
    let file_lines = lines contents
    print $ solvePartOne file_lines
    print $ solvePartTwo file_lines
