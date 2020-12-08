import Text.Parsec
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Map ((!))
import Control.Applicative ((<*))

data BagType = BagType String String deriving (Show, Ord, Eq)

bagTypeP :: Parsec String () BagType
bagTypeP = do
    quality <- many1 letter
    spaces
    color <- many1 letter
    spaces
    string "bag"
    optional $ char 's'
    return $ BagType quality color

type BagQuantity = (Int, BagType)
bagQuantityP :: Parsec String () BagQuantity
bagQuantityP = do
    bagCount <- many1 digit
    spaces 
    bagType <- bagTypeP
    return (read bagCount :: Int, bagType)

type Quantities = S.Set BagQuantity
noBags :: Parsec String () Quantities
noBags = S.empty <$ string "no other bags"
childBags = fmap S.fromList $ sepBy1 bagQuantityP (string ", ")

bagContentsP :: Parsec String () (BagType, Quantities)
bagContentsP = do
    parent <- bagTypeP
    spaces
    string "contain"
    spaces
    children <- noBags <|> childBags
    char '.'
    endOfLine
    return (parent, children)

parseAll txt = case (parse (many1 bagContentsP) "" txt) of
    Left x -> error $ show x
    Right x -> x

type Rules = M.Map BagType (Quantities)
parents :: Rules -> BagType -> S.Set BagType
parents rules t = S.fromList [k | (k, v) <- M.toList rules, t `S.member` (snds v)]
    where snds = S.map snd

possibleOutmostBags :: M.Map BagType Quantities -> BagType -> S.Set BagType
possibleOutmostBags rules = pob pcache where
    pob pcache t = S.unions (immediate:distant) where
        immediate = pcache ! t
        distant = map (pob pcache) . S.toList $ pcache ! t
    -- parents is an expensive search function, so we just run it once for each
    -- bag type and cache the results.
    pcache = M.fromList [(x, parents rules x) | x <- M.keys rules]

part1 text = show . S.size . possibleOutmostBags rules $ BagType "shiny" "gold"
    where rules = M.fromList . parseAll $ text

totalContainedBags :: M.Map BagType Quantities -> BagQuantity -> Int
totalContainedBags rules (n, t) = n*nextLevel where
    nextLevel = sum . map bagCount . S.toList $ (rules ! t)
    bagCount q@(n2, t2) = totalContainedBags rules q + n2

part2 text = show . totalContainedBags rules $ (1, BagType "shiny" "gold")
    where rules = M.fromList . parseAll $ text

main = do
    input <- readFile "day7.txt"
    putStrLn $ part1 input
    putStrLn $ part2 input
