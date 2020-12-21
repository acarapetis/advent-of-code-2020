module Grid (module Grid, bounds, elems, inRange, indices, (!)) where

import Data.Array
import Data.List (intercalate)
import Data.List.Split (chunksOf)

data Cardinal = N | E | S | W deriving (Show, Eq)
reflect N = S
reflect S = N
reflect W = E
reflect E = W

type Coord = (Int, Int)
type Grid = Array Coord

addC :: Coord -> Coord -> Coord
addC (i1, j1) (i2, j2) = (i1 + i2, j1 + j2)

negateC :: Coord -> Coord
negateC (i, j) = (-i, -j)

subtractC :: Coord -> Coord -> Coord
subtractC u v = addC u (negateC v)

scaleC :: Int -> Coord -> Coord
scaleC k (i, j) = (k*i, k*j)

rotateC :: Coord -> Coord
rotateC (i, j) = (j, -i)
rotateC' :: Coord -> Coord
rotateC' (i, j) = (-j, i)

rotateCard N = W
rotateCard W = S
rotateCard S = E
rotateCard E = N

cardToCoord N = (-1, 0)
cardToCoord E = (0, 1)
cardToCoord W = (0, -1)
cardToCoord S = (1, 0)

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

enumerate :: Grid a -> Grid (Coord, a)
enumerate x = listArray (bounds x) $ zip (indices x) (elems x)

directions :: [(Int, Int)]
directions = filter (/= (0,0)) [(i, j)| i <- [-1,0,1], j <- [-1,0,1]]

manhattan :: Coord -> Int
manhattan (i,j) = abs i + abs j

rotateBounds :: (Coord, Coord) -> (Coord, Coord)
rotateBounds (a, b) = ((min i0 i1, min j0 j1), (max i0 i1, max j0 j1)) where
    (i0, j0) = rotateC a
    (i1, j1) = rotateC b

rotateGrid :: Grid a -> Grid a
rotateGrid g = ixmap (rotateBounds $ bounds g) rotateC' g

topEdge :: Grid a -> [a]
topEdge g = take (width g) $ elems g

edge :: Grid a -> Cardinal -> [a]
edge g d
    | d == N = [ g ! (i0, j) | j <- [j0..j1] ]
    | d == S = [ g ! (i1, j) | j <- [j0..j1] ]
    | d == W = [ g ! (i, j0) | i <- [i0..i1] ]
    | d == E = [ g ! (i, j1) | i <- [i0..i1] ]
        where ((i0, j0), (i1, j1)) = bounds g
