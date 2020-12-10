import AOC
import qualified Data.Set as S
import Data.List (find, sort, zipWith, group)

readNums :: String -> [Int]
readNums txt = 0:(map read . lines $ txt)

deltas nums = zipWith (-) (tail nums) nums
numOf x = length . filter (==x)

part1 nums = show $ ((f 3) + 1) * (f 1)
    where f n = numOf n (deltas nums)

-- by inspection, input data has deltas of either 3 or 1;
-- so we just need to take the product of the possible paths
-- through each chain of 1s.

-- number of paths through a chain of n length-1 differences jumps,
-- memoized:
arrC' n
    | n == 0 = 1
    | n == 1 = 1
    | n == 2 = 2
    | otherwise = arrC (n-1) + arrC (n-2) + arrC (n-3)
arrCache = map arrC' [0..]
arrC = (arrCache!!)

get1chains = map length . filter (\x -> head x == 1) . group
part2 = show . product . map arrC . get1chains . deltas

main = do
    nums <- fmap (sort . readNums) $ readFile "day10.txt"
    putStrLn $ part1 nums
    putStrLn $ part2 nums
