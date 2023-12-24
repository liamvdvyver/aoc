module Main where
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Data.Maybe
 
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

solvePartOne :: [String] -> Int
solvePartOne layout = Set.size $ rayTrace layout ((0, 0), (1, 0)) Set.empty
    where
        rayTrace :: [String] -> ((Int, Int), (Int, Int)) -> Set.Set ((Int, Int), (Int, Int)) -> Set.Set (Int, Int)
        rayTrace layout cur hist
            | x < 0 || y < 0 = ret
            | y >= (length layout) || x >= (length $ head layout) = ret
            | cur `Set.member` hist = ret
            | otherwise = Set.unions $ map rayTraceNext nextOffsets
            where

                ((x, y), (vX, vY)) = cur
                ret = Set.fromList $ map fst $ Set.toList hist

                curChar = layout !! y !! x
                nextOffsets = fromJust $ Map.lookup (curChar, (vX, vY)) movement

                rayTraceNext :: (Int, Int) -> Set.Set (Int, Int)
                rayTraceNext (nextVX, nextVY) = rayTrace layout newCur newHist
                    where
                        newCur = ((x + nextVX, y + nextVY), (nextVX, nextVY))
                        newHist = Set.insert cur hist


main = do
    contents <- readFile "16/input.txt"
    let fileLines = lines contents
    print $ solvePartOne fileLines
