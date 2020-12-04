import Text.Parsec
import Control.Applicative ((<*))
import Control.Monad (guard)
import Data.Either (isRight)

data Pair = Pair String String
type Passport = [Pair]

pairChunk = do
    key <- many1 letter
    char ':'
    val <- many1 (choice [digit, letter, char '#'])
    -- Because a single newline can occur WITHIN a passport, but multiple
    -- newlines delimit passports, we have to build in the whitespace here rather
    -- than using "sepBy1 pairChunk spaces".
    optional . many1 $ char ' '
    optional endOfLine
    return $ Pair key val

passport = many1 pairChunk
passports = sepBy1 passport endOfLine

parsePassports :: String -> [Passport]
parsePassports s = case (parse passports "" s) of
    Left x -> error $ show x
    Right x -> x

containsField passport field = any (isField field) passport
isField field (Pair k v) = field == k

requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
hasAllFields passport = all (containsField passport) requiredFields
part1 = show . length . filter hasAllFields

-- Validation is implemented as a Parsec parser with no return value.
-- The field is valid iff parsing is successful.
-- We use guard to implement our custom validation checks.
fieldValidator :: String -> Parsec String () ()
fieldValidator "byr" = do 
    year <- (count 4 digit)
    guard $ read year >= 1920 && read year <= 2002

fieldValidator "iyr" = do 
    year <- (count 4 digit)
    guard $ read year >= 2010 && read year <= 2020

fieldValidator "eyr" = do 
    year <- (count 4 digit)
    guard $ read year >= 2020 && read year <= 2030
    
fieldValidator "hcl" = do 
    char '#'
    count 6 hexDigit
    return ()
    
fieldValidator "ecl" = do 
    x <- count 3 letter
    guard $ any (==x) ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

fieldValidator "pid" = count 9 digit >> return ()

fieldValidator "hgt" = do
    num <- many1 digit
    unit <- choice [string "cm", string "in"]
    guard $ hgtCheck (read num :: Float) unit
    where 
        hgtCheck x "cm" = 150 <= x && x <= 193
        hgtCheck x "in" = 59 <= x && x <= 76

fieldValidator "cid" = many1 anyChar >> return ()
fieldValidator _ = guard False

-- <* eof forces the parser to consume all input
validField (Pair k v) = isRight $ parse (fieldValidator k <* eof) "" v
validPassport pp = hasAllFields pp && all validField pp

part2 = show . length . filter validPassport

main = do
    input <- readFile "day4.txt"
    let passports = parsePassports input in do
        putStrLn $ part1 passports
        putStrLn $ part2 passports
