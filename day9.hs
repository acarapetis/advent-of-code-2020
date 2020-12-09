import AOC
import qualified Data.Set as S
import Data.List (find)

readNums :: String -> [Int]
readNums = map read . lines

isSumOfTwoIn x ys = any (\y -> (y /= x) && ((x-y) `S.member` s)) ys
    where s = S.fromList ys

validAt xs n = x `isSumOfTwoIn` preamble where
    ys = drop n xs
    (preamble, x:_) = splitAt 25 ys

windowAt xs n = preamble where
    ys = drop n xs
    (preamble, x:_) = splitAt 25 ys

firstInvalidIndex xs = fmap (+25) . find (not . validAt xs) $ [25..]
firstInvalid xs = xs !! (fromJust $ firstInvalidIndex xs)

cumSumWindow xs target = firstJust . map matchingWindow $ [0..] where
    matchingWindow n = find matchTarget . takeWhile underTarget $ windows n
    matchTarget = (==target) . sum
    underTarget = (<=target) . sum
    windows n = [window n l | l <- [1..]]
    window n l = take l . drop n $ xs

part1 = show . firstInvalid . readNums
part2 txt = show . firstPlusLast . fromJust
        $ cumSumWindow nums (firstInvalid nums) where
    nums = readNums txt
    firstPlusLast xs = head xs + last xs

main = do
    input <- readFile "day9.txt"
    putStrLn $ part1 input
    putStrLn $ part2 input
