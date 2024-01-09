module Main where

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Text.Read
import qualified Data.Foldable as Set

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

{- | Test plotCorners

>>> plotCorners [(('D', 2), "c") ] [((0, 1), "b"), ((0, 0), "a")]
[((0,3),"c"),((0,1),"b"),((0,0),"a")]
-}
plotCorners :: [((Char, Int), String)] -> [((Int, Int), String)] -> [((Int, Int), String)]
plotCorners [] ret = ret
plotCorners (((dir, dist), hex) : xs) ret = plotCorners xs retNew
  where
    (((lastX, lastY), _) : _) = ret
    (xOffset, yOffset) = fromMaybe (0, 0) $ Map.lookup dir dirMap
    newNode = ((lastX + xOffset * dist, lastY + yOffset * dist), hex)
    retNew = newNode : ret

{- | Test isBoundary

>>> isBoundary [((0, 0), ""), ((0, 3), "")] (0, 2)
True
>>> isBoundary [((0, 0), ""), ((0, 3), "")] (0, 4)
False
>>> isBoundary [((0, 0), ""), ((0, 3), "")] (1, 2)
False
>>> isBoundary [((0, 0), ""), ((0, 3), "")] (1, 4)
False
-}

-- For working with just the corners, can compare pairwise
isBoundary :: [((Int, Int), String)] -> (Int, Int) -> Bool
isBoundary [] (x, y) = False
isBoundary [a] (x, y) = False
isBoundary (((x1, y1), col1) : ((x2, y2), col2) : xs) (x, y)
    | xInside && yInside = True
    | otherwise = isBoundary (((x2, y2), col2) : xs) (x, y)
  where
    xInside = (x - x1) * (x - x2) <= 0
    yInside = (y - y1) * (y - y2) <= 0

-- Flood fill
-- Can memoise later
exteriors ::
    [((Int, Int), String)] ->
    ((Int, Int), (Int, Int)) ->
    Set.Set (Int, Int) ->
    (Int, Int) ->
    Set.Set (Int, Int)
exteriors corners bounds ret (x, y)
    | terminate = ret
    | otherwise = Set.unions $ map (exteriors corners bounds newRet) nexts
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
                || (x, y) `Set.elem` ret
                || isBoundary corners (x, y)
        newRet = Set.insert (x, y) ret

{- | Test part one

>>> input <- readFile "inputs/18p.txt"
>>> let lns = lines input
>>> solvePartOne lns
62
-}
solvePartOne :: [String] -> Int
solvePartOne lns = totalSize - exteriorSize
  where
    corners = plotCorners parsedLns [((0, 0), "")]
    parsedLns = map parseLn lns
    bounds = ((xMin, xMax), (yMin, yMax))
    xMax = maximum $ map (fst . fst) corners
    xMin = minimum $ map (fst . fst) corners
    yMax = maximum $ map (snd . fst) corners
    yMin = minimum $ map (snd . fst) corners
    totalSize = (1 + xMax - xMin) * (1 + yMax - yMin)
    edges = [(x, y) | y <- [yMin .. yMax], x <- [xMin, xMax]] ++ [(x, y) | y <- [yMin, yMax], x <- [xMin .. xMax]]
    exteriorSize = Set.size $ checkEdges corners bounds Set.empty edges

-- Redundant edge starting points can be reached by another starting point, so
-- we can just check whether a starting point is marked as exterior to prune
checkEdges ::
    [((Int, Int), String)] ->
    ((Int, Int), (Int, Int)) ->
    Set.Set (Int, Int) ->
    [(Int, Int)] ->
    Set.Set (Int, Int)
checkEdges corners bounds ret [] = ret
checkEdges corners bounds ret ((x, y) : xs) = checkEdges corners bounds newRet xs
  where
    newRet = Set.union fromCur ret
    fromCur = exteriors corners bounds ret (x, y)

main = do
    contents <- readFile "inputs/18.txt"
    print $ solvePartOne $ lines contents -- IDK why this shit so slow
