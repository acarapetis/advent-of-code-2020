import Data.List (find, sort)
import Data.Maybe (fromMaybe)

readBinBackwards :: [Int] -> Int
readBinBackwards [] = 0
readBinBackwards (x:xs) = x + 2 * (readBinBackwards xs)

trChar 'F' = 0
trChar 'B' = 1
trChar 'L' = 0
trChar 'R' = 1

readOrd :: String -> Int
readOrd = readBinBackwards . reverse . map trChar

data Seat = Seat { row :: Int, col :: Int } deriving Show

parseSeat :: String -> Seat
parseSeat pass = Seat (readOrd r) (readOrd c) 
    where (r, c) = splitAt 7 pass

seatId (Seat row col) = row * 8 + col

part1 = show . maximum . map seatId

missingSeatId seats = 
    let ids = sort . map seatId $ seats 
        gap (a, b) = a + 1 < b
    in do
        (before, after) <- find gap $ zip ids (tail ids)
        return $ before+1

part2 seats = fromMaybe "" $ fmap show (missingSeatId seats)

main = do 
    input <- readFile "day5.txt" 
    let seats = map parseSeat . lines $ input in do
        putStrLn $ part1 seats 
        putStrLn $ part2 seats 
