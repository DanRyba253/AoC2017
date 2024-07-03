module Main where

import           Data.Function        ((&))
import           Data.List            (foldl', scanl')
import           Data.Maybe           (fromMaybe)
import           Data.Void            (Void)
import           Text.Megaparsec      (Parsec, choice, parseMaybe, sepBy)
import           Text.Megaparsec.Char (char, string)

data Move = N | NE | SE | S | SW | NW

type Coords = (Int, Int)

type Parser = Parsec Void String

parseMove :: Parser Move
parseMove = choice
    [ NE <$ string "ne"
    , NW <$ string "nw"
    , N <$ string "n"
    , SE <$ string "se"
    , SW <$ string "sw"
    , S <$ string "s"
    ]

parseMoves :: Parser [Move]
parseMoves = parseMove `sepBy` char ','

processMove :: Coords -> Move -> Coords
processMove (x, y) move = case move of
    N  -> (x,     y + 1)
    NE -> (x + 1, y + 1)
    SE -> (x + 1, y    )
    S  -> (x,     y - 1)
    SW -> (x - 1, y - 1)
    NW -> (x - 1, y    )

countSteps :: Coords -> Int
countSteps (x, y) = if signum x == signum y
    then max (abs x) (abs y)
    else abs x + abs y

part1 :: [Move] -> Int
part1 moves = moves
    & foldl' processMove (0, 0)
    & countSteps

part2 :: [Move] -> Int
part2 moves = moves
    & scanl' processMove (0, 0)
    & map countSteps
    & maximum

main :: IO ()
main = do
    input <- init <$> readFile "input.txt"
    let moves = fromMaybe (error "can't parse input") $
            parseMaybe parseMoves input
    putStrLn $ "Part 1: " ++ show (part1 moves)
    putStrLn $ "Part 2: " ++ show (part2 moves)
