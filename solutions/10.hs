module Main where
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map


-- Given a tile, the 2 (x, y) offsets it maps to
-- The origin is top left
tileToNext = Map.fromList [ ('|', [(0,  1), (0, -1)]),
                            ('-', [(1,  0), (-1, 0)]),
                            ('L', [(1,  0), (0, -1)]),
                            ('J', [(-1, 0), (0, -1)]),
                            ('7', [(-1, 0), (0,  1)]),
                            ('F', [(1,  0), (0,  1)]),
                            ('S', [(1,  0), (-1,  0), (0,  1), (0,  -1)]),
                            ('.', [])
                            ]


-- Given a tile, check if a move is valid
validMove :: [String] -> (Int, Int) -> (Int, Int) -> Bool
validMove sketch (xCurr, yCurr) (xNext, yNext) = moveOffset `elem` tileOffsets
    where
        moveOffset = (xCurr - xNext, yCurr - yNext)
        nextTile = (sketch !! yNext) !! xNext
        tileOffsets :: [(Int, Int)]
        tileOffsets = fromJust $ Map.lookup nextTile tileToNext


findPath :: [String] -> (Int, Int) -> [(Int, Int)] -> Maybe [(Int, Int)]
findPath sketch (xCurr, yCurr) ret
    | currTile == 'S' && length ret > 0 = Just (ret ++ [(xCurr, yCurr)]) -- If back at start, return Just the history
    | null validTiles                   = Nothing   -- If no valid moves from position
    | otherwise                         = head rets -- Recurse over valid moves
    where
        currTile = (sketch !! yCurr) !! xCurr
        (maxX, maxY) = (length $ head sketch, length sketch)
        validOffsets = fromJust $ Map.lookup currTile tileToNext -- Offsets according to value of current tile
        validTiles = filter 
            (\(x, y) ->
                validMove sketch (xCurr, yCurr) (x, y) &&    -- Remove if not valid
                x < maxX && x >= 0 && y < maxY && y >= 0 &&  -- Remove if out of bounds
                (length ret == 0 || last ret /= (x, y))      -- Or if came from here
                ) $ map (\(x, y) -> (x + xCurr, y + yCurr)) validOffsets
        tileRets = map (\(x, y) -> findPath sketch (x, y) (ret ++ [(xCurr, yCurr)])) validTiles -- Returns of visiting each valid node
        rets = filter isJust tileRets


solvePartOne :: [String] -> Integer
solvePartOne sketch = toInteger $ floor $ (toRational $ length $ fromJust path) / 2
    where
        (xStart, yStart) = head [(x, y) | x <- [0..xMax], y <- [0..yMax], ((sketch) !! y) !! x == 'S']
        (xMax, yMax) = ((length $ head sketch) - 1, length sketch - 1)
        path = findPath sketch (xStart, yStart) []

-- Figure out what kind of tile S is
assignS :: [(Int, Int)] -> Char
assignS path
    | length (intersect offsets $ (fromJust $ Map.lookup 'J' tileToNext)) == 2 =  'J'
    | length (intersect offsets $ (fromJust $ Map.lookup 'F' tileToNext)) == 2 =  'F'
    | length (intersect offsets $ (fromJust $ Map.lookup '7' tileToNext)) == 2 =  '7'
    | length (intersect offsets $ (fromJust $ Map.lookup 'L' tileToNext)) == 2 =  'L'
    | length (intersect offsets $ (fromJust $ Map.lookup '-' tileToNext)) == 2 =  '-'
    | length (intersect offsets $ (fromJust $ Map.lookup '|' tileToNext)) == 2 =  '|'
    where
        sCoords = path !! 0
        (adj1, adj2) = (path !! 1, path !! (length path - 2))
        offset (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)
        (offset1, offset2) = (offset adj1 sCoords, offset adj2 sCoords)
        offsets = [offset1, offset2]

replaceS :: [String] -> [(Int, Int)] -> [String]
replaceS sketch path = prevLines ++ [newLine] ++ nextLines
    where
        (xMax, yMax) = ((length $ head sketch) - 1, length sketch - 1)
        (xStart, yStart) = head [(x, y) | x <- [0..xMax], y <- [0..yMax], ((sketch) !! y) !! x == 'S']
        prevLines = take (yStart) sketch
        nextLines = drop (yStart + 1) sketch
        curLine = sketch !! yStart
        prevChars = take (xStart) curLine
        nextChars = drop (xStart + 1) curLine
        curChar = curLine !! xStart
        newChar = assignS path
        newLine = prevChars ++ [newChar] ++ nextChars


-- Make sure queue iterates as you read
-- And pass sketch with S replaced
numInside :: [String] -> Set.Set (Int, Int) -> [(Int, Int)] -> Int -> Bool -> Int -> Int
numInside sketch pipes queue pos prev ret
    | pos == length queue = ret
    | (xCurr, yCurr) `Set.notMember` pipes = checkNext prevInside prevInside -- If we aren't in a pipe, use the previous
    | shouldSwap     = checkNext (not prevInside) False
    | not shouldSwap = checkNext prevInside False
    where

        (xCurr, yCurr) = queue !! pos
        (xPrev, yPrev) = queue !! (pos - 1)
        currTile = (sketch !! yCurr) !! xCurr
        prevTile = (sketch !! yPrev) !! xPrev
        prevInside = if xCurr == 0 then False else prev

        checkNext :: Bool -> Bool -> Int
        checkNext curInside incCount = numInside sketch pipes queue newPos curInside newRet
            where
                newPos = pos + 1
                newRet = if incCount then (ret + 1) else ret

        -- If we know we are on a pipe boundary, should we swap colour
        shouldSwap :: Bool
        shouldSwap
            | currTile == '|' = True
            | currTile == '7' = prevChar == 'L'
            | currTile == 'J' = prevChar == 'F'
            | otherwise       = False
            where
                prevChar = last $ filter (`elem` "LF") $ take xCurr $ sketch !! yCurr

solvePartTwo :: [String] -> Int
solvePartTwo sketch = numInside replacedSketch pipes queue 0 False 0
    where
        (xStart, yStart) = head [(x, y) | x <- [0..xMax], y <- [0..yMax], ((sketch) !! y) !! x == 'S']
        (xMax, yMax) = ((length $ head sketch) - 1, length sketch - 1)
        path = fromJust $ findPath sketch (xStart, yStart) []
        pipes = Set.fromList path
        queue = [ (x, y) | y <- [0..yMax], x <- [0..xMax] ]
        replacedSketch = replaceS sketch path


main = do
    contents <- readFile "inputs/10.txt"
    let file_lines = lines contents
    print $ solvePartOne file_lines
    print $ solvePartTwo file_lines
