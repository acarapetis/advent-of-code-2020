module Grid (module Grid, bounds) where

import Data.Array
import Data.List.Split (chunksOf)

type Coord = (Int, Int)
type Grid = Array Coord

add :: Coord -> Coord -> Coord
add (i1, j1) (i2, j2) = (i1 + i2, j1 + j2)

subtract :: Coord -> Coord -> Coord
subtract (i1, j1) (i2, j2) = (i1 - i2, j1 - j2)

-- This is unsafe: it assumes the elements of xs all have equal length.
fromListOfLists :: [[a]] -> Grid a
fromListOfLists xs = array (min, max) associations where
    min@(i0,j0) = (0,0)
    max@(i1,j1) = (length xs - 1, length (head xs) - 1)
    associations = [((i,j), (xs!!i)!!j) | i <- [i0..i1], j <- [j0..j1]]

width grid = xmax + 1 where (_, (_, xmax)) = bounds grid
height grid = ymax + 1 where (_, (ymax, _)) = bounds grid

toListOfLists :: Grid a -> [[a]]
toListOfLists grid = chunksOf (width grid) $ elems grid
