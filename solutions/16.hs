module Main where

import Control.Monad
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set

import qualified Data.HashTable.IO as H

type HashTable k v = H.BasicHashTable k v

-- Maps (character, offset) to list of offset
-- I.e. current direction to directions for next move
movement :: Map.Map (Char, Velocity) [Velocity]
movement =
    Map.fromList
        [ (('.', (1, 0)), [(1, 0)])
        , (('.', (-1, 0)), [(-1, 0)])
        , (('.', (0, 1)), [(0, 1)])
        , (('.', (0, -1)), [(0, -1)])
        , (('/', (1, 0)), [(0, -1)])
        , (('/', (-1, 0)), [(0, 1)])
        , (('/', (0, 1)), [(-1, 0)])
        , (('/', (0, -1)), [(1, 0)])
        , (('\\', (1, 0)), [(0, 1)])
        , (('\\', (-1, 0)), [(0, -1)])
        , (('\\', (0, 1)), [(1, 0)])
        , (('\\', (0, -1)), [(-1, 0)])
        , (('|', (1, 0)), [(0, -1), (0, 1)])
        , (('|', (-1, 0)), [(0, -1), (0, 1)])
        , (('|', (0, 1)), [(0, 1)])
        , (('|', (0, -1)), [(0, -1)])
        , (('-', (1, 0)), [(1, 0)])
        , (('-', (-1, 0)), [(-1, 0)])
        , (('-', (0, 1)), [(-1, 0), (1, 0)])
        , (('-', (0, -1)), [(-1, 0), (1, 0)])
        ]

type Layout = [String]
type Pos = (Int, Int)
type Velocity = (Int, Int)
type State = (Pos, Velocity)
type History = [State] -- Newest to oldest
type Positions = Set.Set Pos

lookupTile :: Layout -> Pos -> Char
lookupTile layout (x, y) = layout !! y !! x

inBounds :: Layout -> Pos -> Bool
inBounds layout (x, y)
    | x < 0 = False
    | y < 0 = False
    | x >= length (head layout) = False
    | y >= length layout = False
    | otherwise = True

getChildren :: Layout -> State -> [State]
getChildren layout (pos@(x, y), v@(vx, vy)) = validChildren
  where
    nextVs = fromJust $ Map.lookup (lookupTile layout pos, v) movement
    allChildren = map (\(dx, dy) -> ((x + dx, y + dy), (dx, dy))) nextVs
    validChildren = filter (inBounds layout . fst) allChildren

rayTrace ::
    Layout ->
    State ->
    History ->
    HashTable State Positions ->
    IO (Set.Set Pos)
rayTrace layout state@(pos@(x, y), velo@(vx, vy)) history memo = do
    memoised <- H.lookup memo state
    case memoised of
        Just a -> do
            return a
        Nothing -> do
            let tileChar = lookupTile layout pos
            -- let visited = state `elem` history
            let children = getChildren layout state

            case children of
                -- No further children, i.e. terminal node
                [] -> do
                    let ret = Set.singleton pos
                    H.insert memo state ret
                    return ret
                -- Have children to explore
                _ -> do
                    let historyLoop = break ((== pos) . fst) history
                    case historyLoop of
                        -- No loop, just see where each child goes
                        (_, []) -> do
                            childReturns <- mapM (\s -> rayTrace layout s (state : history) memo) children
                            let ret = foldl Set.union (Set.singleton pos) childReturns
                            H.insert memo state ret
                            return ret
                        -- If we have looped to get here
                        (loop, _) -> do
                            -- Children who we have visited in the loop, we know where they lead
                            -- Otherwise, we must still explore them
                            let loopyChild = last loop
                            let unexploredChildren = filter (\child -> fst child /= fst loopyChild) children
                            unexploredChildReturns <- mapM (\s -> rayTrace layout s (state : history) memo) unexploredChildren
                            let ret = foldl Set.union (Set.fromList $ pos : map fst loop) unexploredChildReturns
                            H.insert memo state ret
                            return ret

{- | Part one test input

>>> contents <- readFile "inputs/16p.txt"
>>> let fileLines = lines contents
>>> solutionOne <- solvePartOne fileLines ((0, 0), (1, 0))
>>> print solutionOne
46
-}
solvePartOne' ::
    [String] ->
    ((Int, Int), (Int, Int)) ->
    HashTable ((Int, Int), (Int, Int)) (Set.Set (Int, Int)) ->
    IO Int
solvePartOne' layout start memo = do
    -- Given a memo
    paths <- rayTrace layout start [] memo
    return $ Set.size paths

-- Initialise the memo and pass to helper
solvePartOne :: [String] -> ((Int, Int), (Int, Int)) -> IO Int
solvePartOne layout start = do
    memo <- H.new
    solvePartOne' layout start memo

{- | Part two argmax yields correct value

>>> contents <- readFile "inputs/16p.txt"
>>> let fileLines = lines contents
>>> solutionTwo' <- solvePartOne fileLines ((3, 0), (0, 1))
>>> print solutionTwo'
51
-}

{- | Ensure memo works correctly

>>> contents <- readFile "inputs/16.txt"
>>> let fileLines = lines contents
>>> bigExample <- solvePartTwo' fileLines [((98,0),(0,1))]
>>> print bigExample
8174
>>> bigExample' <- solvePartTwo' fileLines [((0,0),(0,1)),((98,0),(0,1))]
>>> print bigExample'
8174

-}
solvePartTwo' :: [String] -> [State] -> IO Int
solvePartTwo' layout starts = do
    -- Fully memoised approach, doesn't work
    -- memo <- H.new
    -- rets <- mapM (\x -> solvePartOne' layout x memo) starts

    -- Don't share global memo
    rets <- mapM (solvePartOne layout) starts

    return $ maximum rets

{- | Part two test input

>>> contents <- readFile "inputs/16p.txt"
>>> let fileLines = lines contents
>>> solutionTwo <- solvePartTwo fileLines
>>> print solutionTwo
51
-}
solvePartTwo ::
    [String] ->
    IO Int
solvePartTwo layout = do
    let (xMax, yMax) = (length (head layout) - 1, length layout - 1)
    let starts = [((x, 0), (0, 1)) | x <- [0 .. xMax]]
            ++ [((x, yMax), (0, -1)) | x <- [0 .. xMax]]
            ++ [((0, y), (1, 0)) | y <- [0 .. yMax]]
            ++ [((xMax, y), (-1, 0)) | y <- [0 .. yMax]]
    solvePartTwo' layout starts

main = do
    contents <- readFile "inputs/16.txt"
    let fileLines = lines contents
    solutionOne <- solvePartOne fileLines ((0, 0), (1, 0))
    print solutionOne

    solutionTwo <- solvePartTwo fileLines
    print solutionTwo
