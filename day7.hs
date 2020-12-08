import Text.Parsec
import qualified Data.Set as S
import Control.Applicative ((<*))

data BagType = BagType String String deriving Show

bagTypeP :: Parsec String () BagType
bagTypeP = do
    quality <- many1 letter
    spaces
    color <- many1 letter
    spaces
    string "bag"
    optional $ char 's'
    return $ BagType quality color

bagQuantityP = do
    bagCount <- many1 digit
    spaces
    bagType <- bagTypeP
    return (read bagCount :: Int, bagType)

noBags :: Parsec String () [(Int, BagType)]
noBags = [] <$ string "no other bags"
childBags = sepBy1 bagQuantityP (string ", ")

bagContentsP = do
    parent <- bagTypeP
    spaces
    string "contain"
    spaces
    children <- noBags <|> childBags
    char '.'
    return (parent, children)

parseAll = parse (sepBy1 bagContentsP endOfLine) ""

part1 = show . parseAll
part2 _ = ""

main = do
    input <- readFile "day7.txt"
    putStrLn $ part1 input
    putStrLn $ part2 input
