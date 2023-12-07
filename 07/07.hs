module Main where
import Data.List
import Data.Maybe
import qualified Data.Map as Map

-- PART ONE {{{

parseLns :: [String] -> [([Integer], Integer)]
parseLns strs =
    let cards = Map.fromList $ zip "23456789TJQKA" [2..]
        parseHand :: String -> [Integer]
        parseHand hand = map (fromJust . flip Map.lookup cards) hand
        parseBid :: String -> Integer
        parseBid bid = read bid :: Integer
        handTuples = map words strs
    in map (\(hand:bid:_) -> (parseHand hand, parseBid bid)) handTuples


handResult :: ([Integer], Integer) -> (String, [Integer])
handResult (hand, bid) = (handType, handCards)
    where

        handType = getHandType hand
        handCards = getHandCards hand handType

        getHandType :: [Integer] -> String
        getHandType hand = case (countSimilars hand) of
            [5]             -> "5K"
            [4, 1]          -> "4K"
            [3, 2]          -> "FH"
            [3, 1, 1]       -> "3K"
            [2, 2, 1]       -> "2P"
            [2, 1, 1, 1]    -> "1P"
            [1, 1, 1, 1, 1] -> "HC"
            where
                countSimilars :: [Integer] -> [Int]
                countSimilars hand = reverse $ sort $ map length $ group $ sort hand

        getHandCards :: [Integer] -> String -> [Integer]
        getHandCards hand handType
            | handType == "HC" = revHand
            | otherwise        = noSingles
            where
                revHand = reverse $ sort hand
                grpHand = group revHand
                noSingles = concat $ filter ((> 1) . length) grpHand

rankHands :: [([Integer], Integer)] -> [([Integer], Integer)]
rankHands hand = sortBy (compareHands) hand
    where
        compareHands a b = compare (rankableHand a) (rankableHand b)
        handType hand = fst $ handResult hand

        rankableHand :: ([Integer], Integer) -> [Integer]
        rankableHand hand = handRank : fst hand
            where
                result = handResult hand
                handRank = toInteger $ fromJust $ elemIndex (fst result) handsRanked
                handCards = snd result
                handsRanked = ["HC", "1P", "2P" , "3K", "FH", "4K", "5K"]


solvePartOne :: [String] -> Integer
solvePartOne lines = sum $ map (\(bid, rank) -> bid * rank) bidRanks
    where
        bidRanks = zip rankedBids [1..]
        rankedBids = map snd $ rankHands $ parseLns lines

--- }}}


-- PART TWO {{{

parseLns2 :: [String] -> [([Integer], Integer)]
parseLns2 strs =
    let cards = Map.fromList $ zip "J23456789TQKA" [1..]
        parseHand :: String -> [Integer]
        parseHand hand = map (fromJust . flip Map.lookup cards) hand
        parseBid :: String -> Integer
        parseBid bid = read bid :: Integer
        handTuples = map words strs
    in map (\(hand:bid:_) -> (parseHand hand, parseBid bid)) handTuples

addJokers :: Integer -> [Integer] -> [Integer]
addJokers 0 similars = similars
addJokers n [] = [5]
addJokers n [5] = [5]
addJokers n [x] = [x + n]
addJokers n (5:xs) = 5 : addJokers n xs
addJokers n (x:xs) = addJokers (n - 1) ((x + 1):xs)

playJokers :: [Integer] -> [Integer]
playJokers hand =
    let (jokers, jokerless) = partition (== 1) hand
        n = toInteger $ length jokers
        similars = countSimilars jokerless
    in addJokers n similars

countSimilars :: [Integer] -> [Integer]
countSimilars hand = reverse $ sort $ map (toInteger . length) $ group $ sort hand

handResult2 :: ([Integer], Integer) -> (String, [Integer])
handResult2 (hand, bid) = (handType, handCards)
    where
        handType = getHandType hand
        handCards = getHandCards hand handType

        getHandType :: [Integer] -> String
        getHandType hand = case (playJokers hand) of
            [5]             -> "5K"
            [4, 1]          -> "4K"
            [3, 2]          -> "FH"
            [3, 1, 1]       -> "3K"
            [2, 2, 1]       -> "2P"
            [2, 1, 1, 1]    -> "1P"
            [1, 1, 1, 1, 1] -> "HC"

        getHandCards :: [Integer] -> String -> [Integer]
        getHandCards hand handType
            | handType == "HC" = revHand
            | otherwise        = noSingles
            where
                revHand = reverse $ sort hand
                grpHand = group revHand
                noSingles = concat $ filter ((> 1) . length) grpHand

rankHands2 :: [([Integer], Integer)] -> [([Integer], Integer)]
rankHands2 hand = sortBy (compareHands) hand
    where
        compareHands a b = compare (rankableHand a) (rankableHand b)
        handType hand = fst $ handResult hand

        rankableHand :: ([Integer], Integer) -> [Integer]
        rankableHand hand = handRank : fst hand
            where
                result = handResult2 hand
                handRank = toInteger $ fromJust $ elemIndex (fst result) handsRanked
                handCards = snd result
                handsRanked = ["HC", "1P", "2P" , "3K", "FH", "4K", "5K"]

solvePartTwo :: [String] -> Integer
solvePartTwo lines = sum $ map (\(bid, rank) -> bid * rank) bidRanks
    where
        bidRanks = zip rankedBids [1..]
        rankedBids = map snd $ rankHands2 $ parseLns2 lines


--- }}}

main = do
    contents <- readFile "07/input.txt"
    let input = lines contents
    print $ solvePartOne input
    print $ solvePartTwo input
    print $ take 20 $ map handResult $ rankHands $ parseLns input
