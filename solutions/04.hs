module Main where
import System.IO
import Control.Monad

isEmptyStr :: String -> Bool
isEmptyStr "" = True
isEmptyStr " " = True
isEmptyStr a = False

splitOn :: [Char] -> String -> [String]
splitOn delims string = splitOn' delims string 0 []
    where
        splitOn' :: [Char] -> String -> Int -> [String] -> [String]
        splitOn' delims string pos ret
            | pos == length string - 1 = ret ++ [string]
            | string !! pos `elem` delims = do
                let leftPart = map (string !!) [0 .. (pos - 1)]
                let rightPart = map (string !! ) [(pos + 1) .. (length string - 1)]
                splitOn' delims rightPart 0 (ret ++ [leftPart])
            | otherwise = splitOn' delims string (pos + 1) ret

parseLn :: String -> ([Int], [Int])
parseLn line = do
    let split = splitOn ":|" line
    -- let ind = read (last (splitOn " " (split !! 0))) :: Int
    let winners = map (\x -> read x :: Int) (filter (\x -> not $ isEmptyStr x) (splitOn " " (split !! 1)))
    let nums    = map (\x -> read x :: Int) (filter (\x -> not $ isEmptyStr x) (splitOn " " (split !! 2)))
    (winners, nums)

countWins :: ([Int], [Int]) -> Int
countWins (nums, winners) = length (filter (\x -> x `elem` winners) nums)

scoreWins :: Int -> Int
scoreWins 0 = 0
scoreWins x = 2 ^ (x - 1)

solvePartOne :: [String] -> Int
solvePartOne lines = foldl (+) 0 $ map (\x -> scoreWins $ countWins $ parseLn x) lines

initCardQ :: [([Int], [Int])] -> [(Int, ([Int], [Int]))]
initCardQ cards = map (\(a, b) -> (1, (a, b))) cards

incrementCardsQs :: Int -> [Int] -> [(Int, ([Int], [Int]))] -> [(Int, ([Int], [Int]))]
incrementCardsQs n inds cards = incrementIf n inds cards 0
    where
        incrementCardQ :: Int -> (Int, ([Int], [Int])) -> (Int, ([Int], [Int]))
        incrementCardQ n (q, (a, b)) = (q + n, (a, b))

        incrementIf :: Int -> [Int] -> [(Int, ([Int], [Int]))] -> Int -> [(Int, ([Int], [Int]))]
        incrementIf n inds cards pos
            | pos >= length cards = cards
            | not $ pos `elem` inds = incrementIf n inds cards (pos + 1)
            | pos `elem` inds = incrementIf n inds ((map (cards !!) [0..(pos - 1)]) ++ [incrementCardQ n $ cards !! pos] ++ (map (cards !!) [(pos + 1)..(length cards - 1)])) (pos + 1)

winCopies :: [(Int, ([Int], [Int]))] -> [(Int, ([Int], [Int]))]
winCopies cards = winCopies' 0 cards
    where
        winCopies' :: Int -> [(Int, ([Int], [Int]))] -> [(Int, ([Int], [Int]))]
        winCopies' pos cards
            | pos >= length cards = cards
            | otherwise = do
                let curScore =  countWins $ snd (cards !! pos)
                let incNum = fst $ cards !! pos
                let copyInds = tail [ pos + x | x <- [0..curScore] ]
                let cardsNew = incrementCardsQs incNum copyInds cards
                winCopies' (pos + 1) cardsNew

solvePartTwo :: [String] -> Int
solvePartTwo lines = foldl (+) 0 $ map fst (winCopies $ initCardQ $ map parseLn lines)

main :: IO ()
main = do
    file_contents <- readFile "inputs/04.txt"
    let file_lines = lines file_contents
    print $ solvePartOne file_lines
    print $ solvePartTwo file_lines
