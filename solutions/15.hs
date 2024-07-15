module Main where
import Data.Char (ord)
import Data.Maybe
import qualified Data.Map as Map

parseLn :: String -> [String] -> [String]
parseLn [] ret = ret
parseLn str ret = parseLn newStr newRet
    where
        newStr = dropWhile (==',') $ dropWhile (/=',') str
        newRet = ret ++ [takeWhile (`notElem` ['\n', ',']) str]

hashStr :: String -> Int -> Int
hashStr [] ret = ret
hashStr str ret = hashStr (tail str) newRet
    where
        newRet = (`mod` 256) $ (* 17) $ (+ ret) $ ord $ head str

solvePartOne :: String -> Int
solvePartOne str = sum $ map (flip hashStr 0) $ parseLn str []

parseInstruction :: String -> (String, (Char, String))
parseInstruction str = (label, (instruction, focal))
    where
        (label, instruction:focal) = break (`elem` "=-") str

hashAll :: [((String, Int), (Char, String))] -> Map.Map String Int -> [(String, (Char, String))] ->
    [((String, Int), (Char, String))]

hashAll ret hashMap [] = ret
hashAll ret hashMap input = hashAll (ret ++ [((label, hashed), (instruction, focal))]) newHashMap (tail input)  
    where
        (label, (instruction, focal)) = head input
        lookupLabel = Map.lookup label hashMap
        hashed = case lookupLabel of
            Nothing -> hashStr label 0
            Just a -> fromJust lookupLabel
        newHashMap = case lookupLabel of
            Nothing -> Map.insert label hashed hashMap
            Just a -> hashMap

followInstructions :: Map.Map Int [(String, Int)] -> [((String, Int), (Char, String))] -> [(Int, [(String, Int)])]
followInstructions ret [] = Map.toList ret
followInstructions ret instructions = followInstructions newRet (tail instructions)
    where
        ((label, hashed), (instruction, focalStr)) = head instructions
        focal = read focalStr :: Int
        curBoxContents = fromJust $ Map.lookup hashed ret
        newBoxContents = case instruction of
            '-' -> filter ((/=label) . fst) curBoxContents
            '=' ->
                if ((label `notElem`) $ map fst curBoxContents)
                    then curBoxContents ++ [(label, focal)]
                    else fstPart ++ [(label, focal)] ++ tail sndPart
        (fstPart, sndPart) = break ((== label) . fst) curBoxContents
        newRet = Map.insert hashed newBoxContents ret

focusingPower :: Int -> Int -> (String, Int) -> Int
focusingPower boxNo slotNo (label, focal) = (boxNo + 1) * (slotNo) * focal

getFocusingPowers :: (Int, [(String, Int)]) -> [Int]
getFocusingPowers (boxNo, lenses) = zipWith (\slotNo lens -> focusingPower boxNo slotNo lens) [1..] lenses


solvePartTwo str = sum $ map sum $ filter (not . null) $ map getFocusingPowers finalBoxMap
    where
        initBoxMap :: Map.Map Int [(String, Int)]
        initBoxMap = Map.fromList $ zip [0..255] $ repeat []

        instructions = hashAll [] Map.empty $ map parseInstruction $ parseLn str []
        finalBoxMap = followInstructions initBoxMap instructions

-- | Test part one test case
--
-- >> 1 + 4
-- 5

main = do
    contents <- readFile "inputs/15.txt"
    print $ solvePartOne contents
    print $ solvePartTwo contents
