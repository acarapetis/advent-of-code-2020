import AOC
import Asm
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (sort, zipWith, group)

part1 txt = show ""
part2 txt = show ""

data Tile = Floor | EmptySeat | OccupiedSeat
readTile '.' = Floor
readTile 'L' = EmptySeat
readTile '#'= OccupiedSeat

tileP :: Parser Tile
tileP = readTile <$> anyChar
rowP :: Parser [Tile]
rowP = many1 tileP <* optional endOfLine
gridP :: Parser [[Tile]]
gridP = many1 rowP



main = do
    nums <- sort . readNums <$> readFile "day10.txt"
    putStrLn $ part1 nums
    putStrLn $ part2 nums
