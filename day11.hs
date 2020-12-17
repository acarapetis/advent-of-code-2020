module Main where

import AOC
import Grid
import Data.List (find)
import Data.Maybe (fromJust, catMaybes)


data Tile = Floor | EmptySeat | OccupiedSeat deriving (Eq)
instance Show Tile where
    show Floor = "."
    show EmptySeat = "L"
    show OccupiedSeat ="#"

printGrid :: Show a => Grid a -> IO ()
printGrid = putStrLn . showWith (head.show)

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
occFlag OccupiedSeat = 1
occFlag _ = 0
occupiedNeighbours = trimmedConvolve neighbours . fmap occFlag

nextState :: Tile -> Int -> Tile
nextState EmptySeat 0 = OccupiedSeat
nextState OccupiedSeat n = if n>=4 then EmptySeat else OccupiedSeat
nextState x _ = x

step grid = zipGridsWith nextState grid (occupiedNeighbours grid)

final :: (Grid Tile -> Grid Tile) -> Grid Tile -> Grid Tile
final rule grid = fst . fromJust . find same $ zip gs (tail gs) where
    same (g1, g2) = g1 == g2
    gs = iterate rule grid

ray origin direction = iterate (addC direction) origin

firstSeat grid origin direction = let
    segment = takeWhile (inRange $ bounds grid) . tail $ ray origin direction
    tiles = map (grid!) segment
        in find (/=Floor) tiles

seatsInSight grid origin = catMaybes . map (firstSeat grid origin) $ directions
occInSight _ (c, Floor) = 0
occInSight grid (c, _) = sum . map occFlag $ seatsInSight grid c
sightGrid grid = fmap (occInSight grid) (enumerate grid)

step2 grid = zipGridsWith nextState2 grid (sightGrid grid) where
    nextState2 EmptySeat 0 = OccupiedSeat
    nextState2 OccupiedSeat n
        | n>=5 = EmptySeat
        | otherwise = OccupiedSeat
    nextState2 x _ = x

part1 = length . filter (==OccupiedSeat) . elems . final step
part2 = length . filter (==OccupiedSeat) . elems . final step2

main = do
    grid <- parseGrid <$> readFile "day11.txt"
    print $ part1 grid
    print $ part2 grid
