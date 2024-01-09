module Main where

import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Text.Read

dirMap :: Map.Map Char (Int, Int)
dirMap =
    Map.fromList
        [ ('R', (1, 0))
        , ('L', (-1, 0))
        , ('U', (0, -1))
        , ('D', (0, 1))
        ]

{- | Test parsing

>>> ln = "R 6 (#70c710)"
>>> parseLn ln
(('R',6),"#70c710")
-}
parseLn :: String -> ((Char, Int), String)
parseLn ln = ((dir, dist), hex)
  where
    (dir : _) : distRaw : hexRaw : _ = words ln
    hex = drop 1 $ takeWhile (/= ')') hexRaw
    dist = fromMaybe 0 $ readMaybe distRaw

{- | Test plotBoundary

>>> plotBoundary [(('D', 2), "C") ] [((0, 1), "B"), ((0, 0), "A")]
[((0,3),"C"),((0,2),"C"),((0,1),"B"),((0,0),"A")]
-}
plotBoundary :: [((Char, Int), String)] -> [((Int, Int), String)] -> [((Int, Int), String)]
plotBoundary [] ret = ret
plotBoundary (((dir, 0), hex) : xs) ret = plotBoundary xs ret
plotBoundary (((dir, dist), hex) : xs) ret = plotBoundary (((dir, dist - 1), hex) : xs) retNew
  where
    (((lastX, lastY), _) : _) = ret
    (xOffset, yOffset) = fromMaybe (0, 0) $ Map.lookup dir dirMap
    newNode = ((lastX + xOffset, lastY + yOffset), hex)
    retNew = newNode : ret

-- Flood fill
-- Can memoise later
interiors ::
    [(Int, Int)] ->
    ((Int, Int), (Int, Int)) ->
    [(Int, Int)] ->
    (Int, Int) ->
    Set.Set (Int, Int)
interiors boundaries bounds hist (x, y)
    | terminate = Set.fromList hist
    | otherwise = Set.unions $ map (interiors boundaries bounds ((x, y) : hist)) nexts
  where
    nexts =
        [ (x + 1, y)
        , (x - 1, y)
        , (x, y + 1)
        , (x, y - 1)
        ]
    ((xMin, xMax), (yMin, yMax)) = bounds
    terminate =
        x < xMin
            || y < yMin
            || x > xMax
            || y > yMax
            || (x, y) `elem` hist
            || (x, y) `elem` boundaries

{- | Test part one

>>> input <- readFile "inputs/18p.txt"
>>> let lns = lines input
>>> solvePartOne lns
62
-}
solvePartOne :: [String] -> Int
solvePartOne lns = totalSize - exteriorSize
  where
    boundaries = map fst $ plotBoundary parsedLns [((0, 0), "")]
    parsedLns = map parseLn lns
    bounds = ((xMin, xMax), (yMin, yMax))
    xMax = maximum $ map fst boundaries
    xMin = minimum $ map fst boundaries
    yMax = maximum $ map snd boundaries
    yMin = minimum $ map snd boundaries
    totalSize = (1 + xMax - xMin) * (1 + yMax - yMin)
    edges =
        Set.fromList $
            [(x, y) | y <- [yMin .. yMax], x <- [xMin, xMax]] ++ [(x, y) | y <- [yMin, yMax], x <- [xMin .. xMax]]
    exteriorSize = Set.size $ Set.unions $ Set.map (interiors boundaries bounds []) edges

main = do
    input <- readFile "inputs/18.txt"
    print $ solvePartOne $ lines input
