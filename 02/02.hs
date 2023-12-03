module Main where
import System.IO
import Control.Monad

splitLine :: String -> [String]
splitLine line = do
    splitLine' line 0 []
    where
        delims = [": ", "; "]
        splitLine' :: String -> Int -> [String] -> [String]
        splitLine' linePart pos ret
            | pos >= length linePart - 1 = ret ++ [linePart]
            | otherwise = do
                let curCharPair = map (linePart!!) [pos, pos + 1]
                if curCharPair `elem` delims then do
                    let leftPart  = map (linePart!!) [0..(pos - 1)]
                    let rightPart = map (linePart!!) [(pos + 2)..(length linePart - 1)]
                    let retNew = ret ++ [leftPart]
                    splitLine' rightPart 0 retNew
                else splitLine' linePart (pos + 1) ret

gameStrToInd :: String -> Int
gameStrToInd gameStr = read (map (gameStr!!) [5..(length gameStr - 1)]) :: Int

gameStrToCounts :: String -> [Int]
gameStrToCounts gameStr = gameStrToCounts' gameStr 0 []
    where
        gameStrToCounts' :: String -> Int -> [Int] -> [Int]
        gameStrToCounts' gameStr pos ret
            | pos >= (length gameStr) = ret
            | [gameStr!!pos] `elem` map show [0..9] =
                gameStrToCounts' gameStr (pos + 1) ret
            | pos > 0 = do -- This means we just went past a number
                let leftPart = map (gameStr!!) [0..(pos - 1)]
                let rightPart = map (gameStr!!) [pos..(length gameStr - 1)]
                let retNew = ret ++ [(read leftPart :: Int)]
                let posNew = 0
                gameStrToCounts' rightPart posNew retNew
            | pos == 0 =
                gameStrToCounts' (map (gameStr!!) [1..(length gameStr - 1)]) 0 ret


gameStrToColours :: String -> [String]
gameStrToColours gameStr = gameStrToColours' gameStr 0 []
    where
        gameStrToColours' :: String -> Int -> [String] -> [String]
        gameStrToColours' gameStr pos ret
            | pos >= (length gameStr) = ret ++ [gameStr]
            | not $ [gameStr!!pos] `elem` ((map show [0..9]) ++ [",", " ", ";", ":"]) =
                gameStrToColours' gameStr (pos + 1) ret
            | pos > 0 = do -- This means we just went past a non-letter
                let leftPart = map (gameStr!!) [0..(pos - 1)]
                let rightPart = map (gameStr!!) [pos..(length gameStr - 1)]
                let retNew = ret ++ [leftPart]
                let posNew = 0
                gameStrToColours' rightPart posNew retNew
            | pos == 0 =
                gameStrToColours' (map (gameStr!!) [1..(length gameStr - 1)]) 0 ret

gameTuplesToLists :: [([Int], [String])] -> [[(Int, String)]]
gameTuplesToLists tuples = map gameTuplesToLists' tuples
    where
        gameTuplesToLists' (counts, colours) = zip counts colours

parseLine :: [String] -> (Int, [[(Int, String)]])
parseLine (x:xs) = (gameStrToInd x, gameTuplesToLists $ zip (map gameStrToCounts xs) (map gameStrToColours xs))

all_true :: [Bool] -> Bool
all_true bools = foldl (&&) True bools

countFromColour :: [(Int, String)] -> String -> Int
countFromColour totals colour
    | colour `elem` (map snd totals) = fst ((filter (\x -> snd x == colour) totals)!!0)
    | otherwise = 0

arePossibleCounts :: [(Int, String)] -> [(Int, String)] -> Bool
arePossibleCounts counts totals = do
    all_true $ map (\x -> fst x <= (countFromColour totals $ snd x)) counts

isPossibleGame :: [[(Int, String)]] -> [(Int, String)] -> Bool
isPossibleGame counts totals = all_true $ map (\x -> arePossibleCounts x totals) counts

solvePartOne :: [String] -> [(Int, String)] -> Int
solvePartOne lines totals = do
    let parsedGames = map (\x -> parseLine (splitLine x)) lines
    let possibleGames = filter (\x -> (isPossibleGame (snd x) totals)) parsedGames
    let possibleInds = map fst possibleGames
    foldl (+) 0 possibleInds

getMaxCubes :: [[(Int, String)]] -> String -> Int
getMaxCubes counts colour = do
    let allCounts = map (\x -> countFromColour x colour) counts
    foldl max 0 allCounts

getGamePower :: [[(Int, String)]] -> Int
getGamePower counts =
    getMaxCubes counts "red" *
    getMaxCubes counts "green" *
    getMaxCubes counts "blue"

solvePartTwo :: [String] -> Int
solvePartTwo lines = do
    let parsedGames = map (\x -> parseLine (splitLine x)) lines
    let gamePowers = map (\x -> getGamePower (snd x)) parsedGames
    foldl (+) 0 gamePowers

totals = [(12, "red"), (13, "green"), (14, "blue")]

main :: IO ()
main = do
    file_contents <- readFile "02/input.txt"
    let file_lines = lines file_contents
    print $ solvePartOne file_lines totals
    print $ solvePartTwo file_lines
