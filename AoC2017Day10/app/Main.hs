{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import           Data.Bits       (xor)
import           Data.Char       (ord)
import           Data.Function   ((&))
import           Data.List       (intercalate)
import           Data.List.Split (chunksOf)
import           Numeric         (showIntAtBase)

-- Loop datatype and operations on it

data Loop a = Loop
    { elems  :: [a]
    , offset :: Int
    }

rotate :: Int -> Loop a -> Loop a
rotate n (Loop xs off) =
    let n' = n `mod` length xs
        off' = (off + n') `mod` length xs
        xs' = drop n' xs ++ take n' xs
    in  Loop xs' off'

restore :: Loop a -> Loop a
restore (Loop xs off) = rotate (length xs - off) (Loop xs off)

reverseFirst :: Int -> Loop a -> Loop a
reverseFirst n (Loop xs off) =
    let firstN = take n xs
    in  Loop (reverse firstN ++ drop n xs) off

reverseAndRotate :: Int -> Int -> Loop a -> Loop a
reverseAndRotate len skipSize loop = loop
    & reverseFirst len
    & rotate (len + skipSize)

reverseAndRotateAll :: [Int] -> Loop a -> Loop a
reverseAndRotateAll lens loop = foldl (&) loop (zipWith reverseAndRotate lens [0..])

-- input lengths and the starting loop

inputLengths1 :: [Int]
inputLengths1 = [192,69,168,160,78,1,166,28,0,83,198,2,254,255,41,12]

inputLengths2 :: [Int]
inputLengths2 = inputLengths1
    & map (map ord . show)
    & intercalate [ord ',']
    & (++ [17, 31, 73, 47, 23])
    & replicate 64
    & concat

startLoop :: Loop Int
startLoop = Loop [0..255] 0

-- utility functions

toHex :: Int -> String
toHex n = padLeft '0' 2 $ showIntAtBase 16 ("0123456789abcdef" !!) n ""

padLeft :: a -> Int -> [a] -> [a]
padLeft a n xs = replicate (n - length xs) a ++ xs

-- solutions

part1 :: Int
part1 = reverseAndRotateAll inputLengths1 startLoop
    & restore
    & elems
    & take 2
    & product

part2 :: String
part2 = reverseAndRotateAll inputLengths2 startLoop
    & restore
    & elems
    & chunksOf 16
    & concatMap (toHex . foldl1 xor)

main :: IO ()
main = putStrLn part2
