import Data.List (sort, zipWith, group)

readNums txt = 0:(map read . lines $ txt)

deltas nums = zipWith (-) (tail nums) nums
numOf x = length . filter (==x)

part1 nums = show $ (f 3 + 1) * f 1
    where f n = numOf n (deltas nums)

-- by inspection, input data has deltas of either 3 or 1;
-- so we just need to take the product of the possible paths
-- through each chain of 1s.

-- number of paths through a chain of n length-1 differences jumps,
-- memoized:
arrC = (map arrC' [0..] !!) where
    arrC' 0 = 1
    arrC' 1 = 1
    arrC' 2 = 2
    arrC' n = arrC (n-1) + arrC (n-2) + arrC (n-3)

get1chains = map length . filter (\x -> head x == 1) . group
part2 = show . product . map arrC . get1chains . deltas

main = do
    nums <- sort . readNums <$> readFile "day10.txt"
    putStrLn $ part1 nums
    putStrLn $ part2 nums
