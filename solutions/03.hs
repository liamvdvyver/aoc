module Main where
import System.IO
import Control.Monad

nonSymbols = ".1234567890"
nums = "1234567890"

isSymbol :: Char -> Bool
isSymbol char = not $ char `elem` nonSymbols

isNumber :: Char -> Bool
isNumber char = char `elem` nums

coordTuples :: (Int, Int) -> [(Int, Int)]
coordTuples (xmax, ymax) = [ (x, y) | x <- [0..xmax], y <- [0..ymax] ]

borderingChars :: [String] -> (Int, Int) -> [Char]
borderingChars lines (x, y) = do
    let xs = [(max 0 (x - 1))..(min (length (lines!!0) - 1) (x + 1))]
    let ys = [(max 0 (y - 1))..(min (length lines - 1) (y + 1))]
    let inds = [ (x, y) | x <- xs, y <- ys ]
    let borderingChars = map (\(x, y) -> lines!!y!!x) inds
    borderingChars

isBorderingSymbol :: [String] -> (Int, Int) -> Bool
isBorderingSymbol lines (x, y) = do
    let chars = borderingChars lines (x, y)
    let borderingSymbols = map (\x -> not $ x `elem` nonSymbols) chars
    True `elem` borderingSymbols

isNumStart :: [String] -> (Int, Int) -> Bool
isNumStart lines (x, y)
    | x == 0    = isNumber $ lines!!y!!x
    | otherwise = (isNumber $ lines!!y!!x) && not (isNumber $ lines!!y!!(x - 1))

isNumEnd :: [String] -> (Int, Int) -> Bool
isNumEnd lines (x, y)
    | x == (length (lines!!0) - 1) = isNumber $ lines!!y!!x
    | x == 0 = False
    | otherwise = (isNumber $ lines!!y!!x) && not (isNumber $ lines!!y!!(x + 1))

getNumsOnLine :: [String] -> Int -> [((Int, Int), (Int, Int))]
getNumsOnLine lines y = do
    let string = lines!!y
    let starts = map (\x -> isNumStart lines (x, y)) [0..(length string - 1)]
    let ends   = map (\x -> isNumEnd   lines (x, y)) [0..(length string - 1)]
    let startInds = filter (\x -> starts!!x) [0..(length string - 1)]
    let endInds   = filter (\x -> ends!!x)   [0..(length string - 1)]
    let xCoords = zip startInds endInds
    let coords  = map ( \x -> ((fst x, y), (snd x, y)) ) xCoords
    coords

getAllNums :: [String] -> [((Int, Int), (Int, Int))]
getAllNums lines = do
    concat $ map (getNumsOnLine lines) [0..(length lines - 1)]

getNumber :: [String] -> ((Int, Int), (Int, Int)) -> Int
getNumber lines ((xStart, yStart), (xEnd, yEnd)) =
    read (map (lines!!yStart!!) [xStart..xEnd]) :: Int

isBorderingSymbolNum :: [String] -> ((Int, Int), (Int, Int)) -> Bool
isBorderingSymbolNum lines ((xStart, yStart), (xEnd, yEnd)) = do
    let bordering = map (\x -> isBorderingSymbol lines (x, yStart)) [xStart..xEnd]
    True `elem` bordering

solvePartOne :: [String] -> Int
solvePartOne lines = do
    let numCoords = getAllNums lines
    let partNumsCoords = filter (isBorderingSymbolNum lines) numCoords
    let partNums = map (getNumber lines) partNumsCoords
    foldl (+) 0 partNums

boundsToRanges :: [((Int, Int), (Int, Int))] -> [[(Int, Int)]]
boundsToRanges bounds = do
    let ranges = map (\x1 -> (map (\x2 -> (x2, snd $ fst x1)) [(fst $ fst x1)..(fst $ snd x1)])) bounds
    ranges
    -- let ret = concat ranges
    -- ret

getGearsOnLine :: [String] -> Int -> [(Int, Int)]
getGearsOnLine lines y = do
    let string = lines!!y
    let gearsBools = map (\x -> string!!x == '*') [0..(length string - 1)]
    let gearsInds = filter (\x -> gearsBools!!x) [0..(length string - 1)]
    let coords = map ( \x -> (x, y) ) gearsInds
    coords

getAllGears :: [String] -> [(Int, Int)]
getAllGears lines = do
    concat $ map (getGearsOnLine lines) [0..(length lines - 1)]

isBorderingNumGear :: [String] -> (Int, Int) -> Bool
isBorderingNumGear lines (x, y) = do
    let bordering = borderingChars lines (x, y)
    True `elem` (map isNumber bordering)

areAdjacent :: (Int, Int) -> (Int, Int) -> Bool
areAdjacent (x1, y1) (x2, y2) = do
    let diff = (x1 - x2, y1 - y2)
    let adjacentDiffs = [ (x, y) | x <- [-1..1], y <- [-1..1] ]
    diff `elem` adjacentDiffs

getBorderingNums :: [String] -> (Int, Int) -> [((Int, Int), (Int, Int))]
getBorderingNums lines (x, y) = do
    let allNums = getAllNums lines
    let expandedNums = boundsToRanges allNums
    let numElemsAdjacent = map (\x1 -> (map (\x2 -> areAdjacent (x, y) (fst x2, snd x2)) x1)) expandedNums
    let numsAdjacent = map (\x -> foldl (||) False x) numElemsAdjacent
    let returnInds = filter (\x -> numsAdjacent !! x) [0..(length numsAdjacent - 1)]
    let ret = map (allNums !!) returnInds
    ret

gearRatio :: [String] -> (Int, Int) -> Int
gearRatio lines (x, y)
    | length borderingNums /= 2 = 0
    | otherwise = foldl (*) 1 (map (getNumber lines) borderingNums)
    where
        borderingNums = getBorderingNums lines (x, y)

solvePartTwo :: [String] -> Int
solvePartTwo lines = do
    let gears = getAllGears lines
    let ratios = map (gearRatio lines) gears
    foldl (+) 0 ratios

main :: IO ()
main = do
    file_contents <- readFile "inputs/03.txt"
    let file_lines = lines file_contents
    print $ solvePartOne file_lines
    print $ solvePartTwo file_lines
