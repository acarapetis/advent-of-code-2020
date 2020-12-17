module Main where

import AOC
import Grid
import Data.List (find)
import Data.Maybe (fromJust)


data Tile = Floor | EmptySeat | OccupiedSeat deriving (Eq)
instance Show Tile where
    show Floor = "."
    show EmptySeat = "L"
    show OccupiedSeat ="#"

tileP :: Parser Tile
tileP = choice
    [ Floor <$ char '.'
    , EmptySeat <$ char 'L'
    , OccupiedSeat <$ char '#'
    ]
rowP :: Parser [Tile]
rowP = many1 tileP <* optional endOfLine
gridP :: Parser [[Tile]]
gridP = many1 rowP

parseGrid = fromListOfLists . parseOrDie gridP

neighbours = offset (-1, -1) $ fromListOfLists [[1,1,1],[1,0,1],[1,1,1]]
occupiedNeighbours = trimmedConvolve neighbours . fmap occFlag
    where occFlag OccupiedSeat = 1
          occFlag _ = 0

nextState :: Tile -> Int -> Tile
nextState EmptySeat 0 = OccupiedSeat
nextState OccupiedSeat n = if n>=4 then EmptySeat else OccupiedSeat
nextState x _ = x

step grid = zipGridsWith nextState grid (occupiedNeighbours grid)

final :: Grid Tile -> Grid Tile
final grid = fst . fromJust . find same $ zip gs (tail gs) where
    same (g1, g2) = g1 == g2
    gs = iterate step grid

part1 = length . filter (==OccupiedSeat) . elems . final
part2 txt = show ""

main = do
    grid <- parseGrid <$> readFile "day11.txt"
    print $ part1 grid
