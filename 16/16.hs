module Main where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Data.Maybe
import Control.Monad

import qualified Data.HashTable.IO as H
type HashTable k v = H.BasicHashTable k v

 
movement :: Map.Map (Char, (Int, Int)) [(Int, Int)]
movement = Map.fromList [

    (('.', (1 , 0 )), [(1 , 0 )]),
    (('.', (-1, 0 )), [(-1, 0 )]),
    (('.', (0 , 1 )), [(0 , 1 )]),
    (('.', (0 , -1)), [(0 , -1)]),

    (('/', (1 , 0 )), [(0 , -1)]),
    (('/', (-1, 0 )), [(0 ,  1)]),
    (('/', (0 , 1 )), [(-1,  0)]),
    (('/', (0 , -1)), [(1 ,  0)]),

    (('\\', (1 , 0 )), [(0 , 1 )]),
    (('\\', (-1, 0 )), [(0 , -1)]),
    (('\\', (0 , 1 )), [(1 , 0 )]),
    (('\\', (0 , -1)), [(-1, 0 )]),


    (('|', (1 , 0 )), [(0, -1), (0, 1)]),
    (('|', (-1, 0 )), [(0, -1), (0, 1)]),
    (('|', (0 , 1 )), [(0,  1)       ]),
    (('|', (0 , -1)), [(0, -1)       ]),


    (('-', (1 , 0 )), [( 1, 0)         ]),
    (('-', (-1, 0 )), [(-1, 0)         ]),
    (('-', (0 , 1 )), [(-1, 0), (1, 0 )]),
    (('-', (0 , -1)), [(-1, 0), (1, 0 )])

    ]


rayTrace ::
    [String] ->                                                 -- The layout
    ((Int, Int), (Int, Int)) ->                                 -- The current pos and velocity
    Set.Set ((Int, Int), (Int, Int)) ->                         -- All history of pos and v
    HashTable ((Int, Int), (Int, Int)) (Set.Set (Int, Int)) ->  -- Mutable: returned values
    IO (Set.Set (Int, Int))                                     -- All tiles eventually reached

rayTrace layout cur hist memo = do

    lookupVal <- H.lookup memo cur

    let ((x, y), (vX, vY)) = cur
    let shouldExit = (cur `Set.member` hist || x < 0 || y < 0 || y >= (length layout) || x >= (length $ head layout))
    let curRet = Set.fromList $ map fst $ Set.toList hist
    let curChar = layout !! y !! x

    let nextOffsets = fromJust $ Map.lookup (curChar, (vX, vY)) movement

    let rayTraceNext (nextVX, nextVY) = rayTrace layout newCur newHist memo
            where
                newCur = ((x + nextVX, y + nextVY), (nextVX, nextVY))
                newHist = Set.insert cur hist

    case (isJust lookupVal) of
        True  -> (return $ Set.union (fromJust lookupVal) (curRet))
        False ->
            case shouldExit of
                True  -> do
                    H.insert memo cur curRet
                    return curRet
                False -> do
                    nextPaths <- mapM rayTraceNext nextOffsets
                    return $ Set.unions $ nextPaths


solvePartOne :: [String] -> IO Int
solvePartOne layout = do
    memo <- H.new
    paths <- rayTrace layout ((0, 0), (1, 0)) Set.empty memo
    return $ Set.size paths


main = do
    contents <- readFile "16/input.txt"
    let fileLines = lines contents
    solution <- solvePartOne fileLines
    print solution
