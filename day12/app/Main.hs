module Main where

import           Data.List   (find, foldl')
import           Data.Set    (Set)
import qualified Data.Set    as S
import           Data.Vector (Vector)
import qualified Data.Vector as V

connected :: Vector [Int] -> Set Int -> Int -> Set Int
connected graph visited current
    | S.member current visited = visited
    | otherwise = foldl' (connected graph) (S.insert current visited) (graph V.! current)

groupCount :: Vector [Int] -> Set Int -> Int -> Int
groupCount graph visited current = 1 + maybe 0
    (groupCount graph visited')
    (find (`S.notMember` visited') allNodes)
  where
    visited' = connected graph visited current
    allNodes = [0 .. V.length graph - 1]

main :: IO ()
main = do
    graph <- V.fromList . read <$> readFile "input.txt"
    putStrLn $ "Part 1: " ++ show (S.size $ connected graph S.empty 0)
    putStrLn $ "Part 2: " ++ show (groupCount graph S.empty 0)
