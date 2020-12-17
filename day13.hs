import AOC
import Data.List.Split
import Data.List
import Data.Maybe

parse13 text = (read $ l!!0, splitOn "," $ l!!1) where l = lines text

argmax f = fst . maximumBy cmpf . map (\x -> (x, f x))
    where cmpf (_, i) (_, j) = compare i j

argmin :: Ord b => (a -> b) -> [a] -> a
argmin f = fst . minimumBy cmpf . map (\x -> (x, f x))
    where cmpf (_, i) (_, j) = compare i j

busses :: [String] -> [Int]
busses = map read . filter (/="x")

part1 :: Int -> [String] -> Int
part1 startTime services = waitTime * bestBus where
    waitTime = bestBus - (startTime `mod` bestBus)
    bestBus = argmax (startTime `mod`) . busses $ services

-- Chinese Remainder Theorem... is this projecteuler now?
-- solve a1 === x mod n1, a2 === x mod n2 simultaneously
sieve (a1, n1) (a2, n2) = (x, n1*n2) where
    x = head . filter (\x -> x `mod` n2 == a2) $ iterate (+n1) a1

busses2 :: [String] -> [(Integer, Integer)]
busses2 = map neg . catMaybes . map read' . zip [0,1..] where
    read' (_, "x") = Nothing
    read' (n, x) = Just (n, read x)
    neg (a, n) = (n-a `mod` n, n)

part2 = fst . foldl1 sieve . busses2

main = do
    (startTime, services) <- parse13 <$> readFile "day13.txt"
    print $ part1 startTime services
    print $ part2 services
