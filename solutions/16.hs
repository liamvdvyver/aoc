module Main where

import Control.Monad
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set

import qualified Data.HashTable.IO as H

type HashTable k v = H.BasicHashTable k v

movement :: Map.Map (Char, (Int, Int)) [(Int, Int)]
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

rayTrace ::
    [String] -> -- The layout
    ((Int, Int), (Int, Int)) -> -- The current pos and velocity
    Set.Set ((Int, Int), (Int, Int)) -> -- All history of pos and v
    HashTable ((Int, Int), (Int, Int)) (Set.Set (Int, Int)) -> -- Mutable: returned values
    IO (Set.Set (Int, Int)) -- All tiles eventually reached
rayTrace layout cur hist memo = do
    lookupVal <- H.lookup memo cur

    let ((x, y), (vX, vY)) = cur
    let shouldExit = cur `Set.member` hist || x < 0 || y < 0 || y >= length layout || x >= length (head layout)
    let curRet = Set.fromList $ map fst $ Set.toList hist
    let curChar = layout !! y !! x

    let nextOffsets = fromJust $ Map.lookup (curChar, (vX, vY)) movement

    let rayTraceNext (nextVX, nextVY) = rayTrace layout newCur newHist memo
          where
            newCur = ((x + nextVX, y + nextVY), (nextVX, nextVY))
            newHist = Set.insert cur hist

    ( if isJust lookupVal
            then return $ Set.union (fromJust lookupVal) curRet
            else
                ( if shouldExit
                    then
                        ( do
                            H.insert memo cur curRet
                            return curRet
                        )
                    else
                        ( do
                            nextPaths <- mapM rayTraceNext nextOffsets
                            return $ Set.unions $ nextPaths
                        )
                )
        )

solvePartOne :: [String] -> ((Int, Int), (Int, Int)) -> IO Int
solvePartOne layout start = do
    memo <- H.new
    paths <- rayTrace layout start Set.empty memo
    return $ Set.size paths

{- | Part one test input

>>> contents <- readFile "inputs/16p.txt"
>>> let fileLines = lines contents
>>> solutionOne <- solvePartOne fileLines ((0, 0), (1, 0))
>>> print solutionOne
46
-}
solvePartTwo :: [String] -> IO Int
solvePartTwo layout = do
    let starts =
            [((x, 0), (0, 1)) | x <- [0 .. ((length . head) layout - 1)]]
                ++ [((x, length layout - 1), (0, -1)) | x <- [0 .. ((length . head) layout - 1)]]
                ++ [((0, y), (1, 0)) | y <- [0 .. (length layout - 1)]]
                ++ [((length layout - 1, y), (-1, 0)) | y <- [0 .. (length layout - 1)]]
    rets <- mapM (solvePartOne layout) starts
    return $ maximum rets

{- | Part two argmax yields correct value

>>> contents <- readFile "inputs/16p.txt"
>>> let fileLines = lines contents
>>> solutionTwo' <- solvePartOne fileLines ((3, 0), (0, 1))
>>> print solutionTwo'
51
-}

{- | Part two test input

>>> contents <- readFile "inputs/16p.txt"
>>> let fileLines = lines contents
>>> solutionTwo <- solvePartTwo fileLines
>>> print solutionTwo
51
-}
main = do
    contents <- readFile "inputs/16.txt"
    let fileLines = lines contents
    solutionOne <- solvePartOne fileLines ((0, 0), (1, 0))
    print solutionOne
    solutionTwo <- solvePartTwo fileLines
    print solutionTwo
