module Grid (module Grid, bounds, elems) where

import Data.Array
import Data.List (intercalate)
import Data.List.Split (chunksOf)

type Coord = (Int, Int)
type Grid = Array Coord

addC :: Coord -> Coord -> Coord
addC (i1, j1) (i2, j2) = (i1 + i2, j1 + j2)

negateC :: Coord -> Coord
negateC (i, j) = (-i, -j)

subtractC :: Coord -> Coord -> Coord
subtractC u v = addC u (negateC v)

-- This is unsafe: it assumes the elements of xs all have equal length.
fromListOfLists :: [[a]] -> Grid a
fromListOfLists xs = array (min, max) associations where
    min = (0,0)
    max = (length xs - 1, length (head xs) - 1)
    associations = [((i,j), (xs!!i)!!j) | (i,j) <- range (min, max)]

width grid = xmax - xmin + 1 where ((_, xmin), (_, xmax)) = bounds grid
height grid = ymax - ymin + 1 where ((ymin, _), (ymax, _)) = bounds grid

toListOfLists :: Grid a -> [[a]]
toListOfLists grid = chunksOf (width grid) $ elems grid

showWith :: (a -> Char) -> Grid a -> String
showWith f = intercalate "\n" . map (map f) . toListOfLists

offset :: Coord -> Grid a -> Grid a
offset delta grid = ixmap (addC start delta, addC end delta) (`subtractC` delta) grid
    where (start, end) = bounds grid

convolveWith ::
    (a -> b -> c)        -- function to combine values sampled from the two grids
    -> (c -> c -> c)     -- function to accumulate the combined values
    -> c                 -- starting value for each accumulator
    -> Grid a -> Grid b  -- input grids
    -> Grid c
convolveWith combine accum start f g = accumArray accum start (min, max) associations where
    min = addC minf ming
    max = addC maxf maxg
    (minf, maxf) = bounds f
    (ming, maxg) = bounds g
    associations = [((i0 + i1, j0 + j1), combine (f ! (i0, j0)) (g ! (i1, j1))) |
        i0 <- [fst minf..fst maxf], j0 <- [snd minf..snd maxf],
        i1 <- [fst ming..fst maxg], j1 <- [snd ming..snd maxg]]

-- traditional linear convolution
convolve :: Num a => Grid a -> Grid a -> Grid a
convolve = convolveWith (*) (+) 0

trim :: (Coord, Coord) -> Grid a -> Grid a
trim (min, max) = array (min, max) . filter inBounds . assocs
    where inBounds (c, _) = inRange (min, max) c

trimmedConvolveWith ::
    (a -> b -> c) -> (c -> c -> c) -> c -- as in convolveWith
    -> Grid a -- KERNEL grid
    -> Grid b -- DATA grid, output will be the same size
    -> Grid c
trimmedConvolveWith combine accum start kernel dat
    = trim (bounds dat) $ convolveWith combine accum start kernel dat

trimmedConvolve :: Num a => Grid a -> Grid a -> Grid a
trimmedConvolve = trimmedConvolveWith (*) (+) 0

zipGridsWith :: (a -> b -> c) -> Grid a -> Grid b -> Grid c
zipGridsWith f x y
    | bounds x /= bounds y = error "Incompatible grids"
    | otherwise = listArray (bounds x) $ zipWith f (elems x) (elems y)
