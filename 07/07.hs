module Main where
import Data.List
import Data.Maybe
import qualified Data.Map as Map


-- Swap out 'J' for 'j'
swapJokers :: [String] -> [String]
swapJokers lines = map (map (\c -> if c == 'J' then 'j' else c)) lines

parseLns :: [String] -> [([Integer], Integer)]
parseLns strs =
    let cards = Map.fromList $ zip "j23456789TJQKA" [1..]

        parseHand :: String -> [Integer]
        parseHand hand = map (fromJust . flip Map.lookup cards) hand

        parseBid :: String -> Integer
        parseBid bid = read bid :: Integer

        handTuples = map words strs

    in map (\(hand:bid:_) -> (parseHand hand, parseBid bid)) handTuples


-- Counts of each unique card in the optimal hand (considering jokers)
countSimilars :: [Integer] -> [Integer]
countSimilars hand =
    addJokers n similars
    where
        (jokers, jokerless) = partition (== 1) hand
        n = toInteger $ length jokers
        similars = countSimilars' jokerless

        -- Add jokers to increase the highest count of similar cards
        -- Up to 5
        addJokers :: Integer -> [Integer] -> [Integer]
        addJokers 0 similars = similars
        addJokers n [] = [5]
        addJokers n [5] = [5]
        addJokers n [x] = [x + n]
        addJokers n (5:xs) = 5 : addJokers n xs
        addJokers n (x:xs) = addJokers (n - 1) ((x + 1):xs)

        -- Counts of unique cards in a hand
        countSimilars' :: [Integer] -> [Integer]
        countSimilars' hand = reverse $ sort $ map (toInteger . length) $ group $ sort hand


getHandType :: [Integer] -> String
getHandType hand = case (countSimilars hand) of
    [5]             -> "5K"
    [4, 1]          -> "4K"
    [3, 2]          -> "FH"
    [3, 1, 1]       -> "3K"
    [2, 2, 1]       -> "2P"
    [2, 1, 1, 1]    -> "1P"
    [1, 1, 1, 1, 1] -> "HC"


handResult :: ([Integer], Integer) -> (String, [Integer])
handResult (hand, bid) = (handType, hand)
    where handType  = getHandType hand


rankHands :: [([Integer], Integer)] -> [([Integer], Integer)]
rankHands hand = sortBy (compareHands) hand
    where
        compareHands a b = compare (rankableHand a) (rankableHand b)
        handType hand = fst $ handResult hand

        -- Get list of hand type ranking, then cards in hand
        -- To compare lexicographically
        rankableHand :: ([Integer], Integer) -> [Integer]
        rankableHand hand = handRank : fst hand
            where
                result = handResult hand
                handRank = toInteger $ fromJust $ elemIndex (fst result) ranks
                handCards = snd result
                ranks = ["HC", "1P", "2P" , "3K", "FH", "4K", "5K"]


solvePart :: Integer -> [String] -> Integer
solvePart part lines = sum $ map (\(bid, rank) -> bid * rank) bidRanks
    where
        bidRanks = zip rankedBids [1..]
        rankedBids = map snd $ rankHands parsed
        parsed = case part of 1 -> parseLns lines
                              2 -> parseLns $ swapJokers lines


main = do
    contents <- readFile "07/input_partial.txt"
    let input = lines contents
    print $ solvePart 1 $ input
    print $ solvePart 2 $ input
