import AOC
import qualified Data.IntMap as M
import Data.List (stripPrefix)
import Data.Maybe (maybeToList)

data Rule = Str String | RuleRefs [Int] | Alt Rule Rule deriving (Show, Eq)
type Rules = M.IntMap Rule

problemP :: Parser (Rules, [String])
problemP = do
    rules <- rulesP
    endOfLine
    inputs <- many1 (manyTill anyChar $ try endOfLine)
    return (rules, inputs)

children (Alt a b) = children a ++ children b
children (RuleRefs xs) = xs
children _ = []

rulesP :: Parser Rules
rulesP = M.fromList <$> many1 ruleLineP
ruleLineP :: Parser (Int, Rule)
ruleLineP = do
    num <- many1 digit
    char ':'
    spaces
    rule <- ruleP
    endOfLine
    return (read num ,rule)
ruleP = choice [strP, try altP, refsP]
strP = do
    char '"'
    s <- many1 (noneOf "\"")
    char '"'
    return $ Str s
refsP = RuleRefs . map read <$> many1 (many1 digit <* optional (char ' '))
altP = do
    a <- refsP
    optional $ char ' '
    string "| "
    b <- refsP
    return $ Alt a b

-- Basically implementing my own terrible version of Parsec (minus the return
-- values; so just a "validating parser") so that I can handle circular
-- dependencies for part 2.
remainders :: Rules -- Ruleset to parse with
    -> Int -- Number of rule to apply
    -> String -- Input string
    -> [String] -- Possible tails remaining after parsing
remainders rules i = match' (rules M.! i) where
    match' (Str s) xs = maybeToList $ stripPrefix s xs
    match' (Alt a b) xs = match' a xs ++ match' b xs
    match' (RuleRefs rs) xs = matchSeq rs xs where
        matchSeq :: [Int] -> String -> [String]
        matchSeq [] xs = [xs]
        matchSeq (i:is) xs = concat . map (matchSeq is) $ remainders rules i xs

matches :: Rules -> Int -> String -> Bool
matches rules i x = any (=="") $ remainders rules i x

part1 txt = length . filter (matches rules 0) $ inputs where
    (rules, inputs) = parseOrDie problemP txt

fixRules = M.union . parseOrDie rulesP $ unlines ["8: 42 | 42 8","11: 42 31 | 42 11 31"]

part2 txt = length . filter (matches rules 0) $ inputs where
    (oldRules, inputs) = parseOrDie problemP txt
    rules = fixRules oldRules

main = do
    input <- readFile "day19.txt"
    print $ part1 input
    print $ part2 input
