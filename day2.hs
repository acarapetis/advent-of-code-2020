module Day2 (PWSet, pwLine, valid1, valid2, parsePWLine) where

import Text.Parsec
data PWSet = PWSet {
    min :: Int, 
    max :: Int,
    constrainedChar :: Char,
    password :: String
} deriving Show

pwLine :: Parsec String () PWSet
pwLine = do
    min <- many1 digit
    char '-'
    max <- many1 digit
    spaces
    constrainedChar <- anyChar
    char ':'
    spaces 
    password <- many1 anyChar
    return $ PWSet (read min) (read max) constrainedChar password

count x = length . filter (==x)

valid1 :: PWSet -> Bool
valid1 (PWSet min max cchar pw) = 
    let ccount = length (filter (==cchar) pw) in
        ccount >= min && ccount <= max

part1 :: [PWSet] -> String
part1 = show . length . filter valid1

charMatch pw pos chr = (pw !! (pos-1)) == chr
valid2 (PWSet min max cchar pw) = charMatch pw min cchar /= charMatch pw max cchar
part2 = show . length . filter valid2

parsePWLine l = case (parse pwLine "" l) of
    Left _ -> error "Bad password line"
    Right x -> x

main = do
    input <- getContents
    let pwsets = map (parsePWLine) (lines input) in do
            putStrLn $ part1 pwsets
            putStrLn $ part2 pwsets
