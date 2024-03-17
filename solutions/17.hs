module Main where

import Data.Maybe
import qualified Data.PSQueue as P
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (zipWith5)

type Layout = [[Int]]
type Position = (Int, Int)
type Velocity = (Int, Int)
type PV = (Position, Velocity)
type State = (PV, Int) -- Store direction in a straight line
type Path = [State]

parseInput :: String -> Layout
parseInput input = map (map (read . pure)) inputLines
  where
    inputLines = lines input

{- | Get legal moves from a position

>>> inputRaw <- readFile "inputs/17p.txt"
>>> let input = parseInput inputRaw
>>> length $ legalMoves input 3 (((1,1),(1,0)), 2)
3
>>> length $ legalMoves input 3 (((1,1),(1,0)), 3)
2
>>> length $ legalMoves input 3 (((0,0),(1,0)), 2)
2
-}
legalMoves :: Layout -> (Int, Int) -> State -> [State]
legalMoves layout (min, max) vertex@(((x, y), velocity), lineLength) = filter inBounds $ filter notTooShort $ filter notTooLong nextMoves
  where
    nextMoves :: [State]
    nextMoves = [(((x + dx, y + dy), (dx, dy)), newLength) | (dx, dy, newLength) <- offsets]
      where
        offsets
            | velocity == (0, 0) = [(1, 0, 1), (-1, 0, 1), (0, 1, 1), (0, -1, 1)]
            | velocity == (1, 0) = [(1, 0, lineLength + 1), (0, 1, 1), (0, -1, 1)]
            | velocity == (-1, 0) = [(-1, 0, lineLength + 1), (0, 1, 1), (0, -1, 1)]
            | velocity == (0, 1) = [(0, 1, lineLength + 1), (1, 0, 1), (-1, 0, 1)]
            | velocity == (0, -1) = [(0, -1, lineLength + 1), (1, 0, 1), (-1, 0, 1)]
            | otherwise = error "bad velocity"

    notTooLong :: State -> Bool
    notTooLong ((_, childVelocity), _)
        | lineLength < max = True
        | childVelocity == velocity = False
        | otherwise = True

    notTooShort :: State -> Bool
    notTooShort ((_, childVelocity), _)
        | lineLength >= min = True
        | childVelocity == velocity = True
        | velocity == (0, 0) = True
        | otherwise = False

    inBounds :: State -> Bool
    inBounds (((x, y), _), _)
        | x < 0 = False
        | y < 0 = False
        | x > xMax = False
        | y > yMax = False
        | otherwise = True
      where
        (xMax, yMax) = (length (head layout) - 1, length layout - 1)

heuristic :: Position -> State -> Int
heuristic (targetX, targetY) (((x, y), _), _) = abs (targetX - x) + abs (targetY - y)

getPartialHeatLoss :: Layout -> Path -> Int
getPartialHeatLoss layout states = sum $ map (blockHeatLoss layout) states

blockHeatLoss :: Layout -> State -> Int
blockHeatLoss layout (((x, y), _), _) = layout !! y !! x

cost :: Layout -> Position -> Path -> Int -- Include current code in history
cost layout goal path@(cur : history) = getPartialHeatLoss layout path + heuristic goal cur

-- Can I do this by position, or by PV?
-- For now, start doing it by full State
-- But this could maybe be optimised
aStarSearch :: Layout -> Position -> (Int, Int) -> P.PSQ State Int -> M.Map State Path -> M.Map State Int -> M.Map State Int -> Path
aStarSearch layout goal (min, max) frontier paths partialHeatLosses heuristicCosts
    | (fst . fst) cur == goal && snd cur >= min = fromJust $ M.lookup cur paths
    | otherwise = aStarSearch layout goal (min, max) newFrontier newPaths newPartialHeatLosses newHeuristicCosts
  where
    cur = P.key $ fromJust $ P.findMin frontier
    curPartialHeatLoss = fromJust $ M.lookup cur partialHeatLosses
    curPath = fromJust $ M.lookup cur paths

    neighbours = legalMoves layout (min, max) cur

    -- Costs of reaching each neighbour through the current node
    -- If is is the best path to each neighbour, update:
    --
    -- - best paths to these neighbours
    -- - best partial heat losses
    -- - best heuristic costs
    -- - frontier (even if already visited)

    neighbourPaths :: [Path]
    neighbourPaths = map (:curPath) neighbours

    neighbourPreviousHeatLosses :: [Maybe Int]
    neighbourPreviousHeatLosses = map (\state -> M.lookup state partialHeatLosses) neighbours

    neighbourPartialHeatLosses :: [Int]
    neighbourPartialHeatLosses = map (getPartialHeatLoss layout) neighbourPaths

    neighbourHeuristicCosts = zipWith (\state loss -> loss + heuristic goal state) neighbours neighbourPartialHeatLosses

    neighbourBestPath = zipWith compareLosses neighbourPreviousHeatLosses neighbourPartialHeatLosses
        where
            compareLosses :: Maybe Int -> Int -> Bool
            compareLosses Nothing b = True
            compareLosses (Just a) b
                | b < a = True
                | otherwise = False


    -- Keep these values only for those neighbours for which we have found the
    -- best path
    neighboursOnlyBest :: [(State, Path, Int, Int, Bool)]
    neighboursOnlyBest = filter (\(_,_,_,_,b) -> b) $ zipWith5 (\a b c d e -> (a, b, c, d, e)) neighbours neighbourPaths neighbourPartialHeatLosses neighbourHeuristicCosts neighbourBestPath

    newFrontier = foldl (\psq (state, _,_,cost,_) -> P.insert state cost psq) (P.delete cur frontier) neighboursOnlyBest
    newPaths = foldl (\map (state,path,_,_,_) -> M.insert state path map) paths neighboursOnlyBest
    newPartialHeatLosses = foldl (\map (state,_,heatLoss,_,_) -> M.insert state heatLoss map) partialHeatLosses neighboursOnlyBest
    newHeuristicCosts = foldl (\map (state,_,_,heuristicCost,_) -> M.insert state heuristicCost map) heuristicCosts neighboursOnlyBest

{- Test part one

>>> input <- readFile "inputs/17p.txt"
>>> let layout = parseInput input
>>> solvePartOne layout
102
-}
solvePartOne :: Layout -> Position -> Int
solvePartOne layout entry = getPartialHeatLoss layout bestPath
    where
        goal = (length (head layout) - 1, length layout - 1)
        bestPath = aStarSearch layout goal (0, 3) initialFrontier initialPaths initialHeatLosses initialHeuristics
        initialFrontier = P.singleton initialState 0
        initialState :: State
        initialState = ((entry, (0, 0)), 0)
        initialPaths = M.singleton initialState []
        initialHeatLosses = M.singleton initialState 0
        initialHeuristics = M.singleton initialState $ heuristic goal initialState

solvePartTwo :: Layout -> Position -> Int
solvePartTwo layout entry = getPartialHeatLoss layout bestPath
    where
        goal = (length (head layout) - 1, length layout - 1)
        bestPath = aStarSearch layout goal (4, 10) initialFrontier initialPaths initialHeatLosses initialHeuristics
        initialFrontier = P.singleton initialState 0
        initialState :: State
        initialState = ((entry, (0, 0)), 0)
        initialPaths = M.singleton initialState []
        initialHeatLosses = M.singleton initialState 0
        initialHeuristics = M.singleton initialState $ heuristic goal initialState

main = do
    rawInput <- readFile "inputs/17.txt"
    let input = parseInput rawInput
    print $ solvePartOne input (0, 0)
    print $ solvePartTwo input (0, 0)
