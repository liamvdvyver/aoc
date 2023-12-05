module SplitOn where

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
