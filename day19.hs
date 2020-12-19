import AOC
import Data.Graph
import qualified Data.IntMap as M
import Data.Either (isRight)

part1 txt = length . filter (validate rules) $ inputs
    where (rules, inputs) = parseOrDie problemP txt

problemP :: Parser (Rules, [String])
problemP = do
    rules <- rulesP
    endOfLine
    inputs <- many1 (manyTill anyChar $ try endOfLine)
    return (rules, inputs)

data Rule = Str String | RuleRefs [Int] | Alt Rule Rule deriving (Show, Eq)
type Rules = M.IntMap Rule

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

validate :: Rules -> String -> Bool
validate rules = isRight . parse (masterParser rules <* eof)

type RuleGraph = (Graph, Vertex -> (Rule, Int, [Int]), Int -> Maybe Vertex)

masterParser :: Rules -> Parser String
masterParser rules = (assembleParsers graph ordering) M.! 0 where
    ordering = topSort . transposeG $ g
    graph@(g,_,_) = buildGraph rules

buildGraph :: Rules -> RuleGraph
buildGraph = graphFromEdges . map f . M.toAscList
    where f (i, x) = (x, i, children x)

assembleParsers :: RuleGraph -> [Vertex] -> M.IntMap (Parser String)
assembleParsers (_, vertData, _) = foldl addParser M.empty where
    addParser ps v = M.insert key value ps where
        (rule, key, kids) = vertData v
        value = buildParser ps rule

buildParser :: M.IntMap (Parser String) -> Rule -> Parser String
buildParser ps (Str s) = string s
buildParser ps (RuleRefs xs) = concat <$> foldr andThen (return []) xs where
    andThen :: Int -> Parser [String] -> Parser [String]
    andThen i acc = do
        v <- (ps M.! i)
        w <- acc
        return $ v:w
buildParser ps (Alt r1 r2) = try (buildParser ps r1) <|> buildParser ps r2

main = do
    input <- readFile "day19.txt"
    print $ part1 input
