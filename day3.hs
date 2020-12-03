type Grid = [[Bool]]
parseGrid :: String -> Grid
parseGrid = map parseLine . lines
parseLine = map parseChar
parseChar :: Char -> Bool
parseChar '.' = False
parseChar '#' = True
parseChar x = error $ "Invalid Character: " ++ [x]

width grid = length $ (grid!!0)
hasTree grid (y,x) = (grid !! y) !! (x `mod` (width grid))
path (dy, dx) grid = map (\x -> (x*dy, x*dx)) [1..(div (length grid) dy) - 1]
treeCount slope grid = length . filter (hasTree grid) . path slope $ grid

part1 = show . treeCount (1,3) . parseGrid

treeCounts :: Grid -> [Int]
slopes = [(1,1), (1,3), (1,5), (1,7), (2,1)]
treeCounts grid = map (flip treeCount grid) $ slopes
part2 = show . foldl (*) 1 . treeCounts . parseGrid

main = do
    input <- readFile "day3.txt"
    putStrLn $ part1 input
    putStrLn $ part2 input
