import AOC
import qualified Data.Map as M

data FieldRule = FR String (Int -> Bool)
type Ticket = [Int]

instance Eq FieldRule where FR x _ == FR y _ = x == y
instance Show FieldRule where show (FR x _) = x

fieldRuleP :: Parser FieldRule
fieldRuleP = do
    field <- many1 (letter <|> char ' ')
    string ": "
    r1 <- rangeP
    string " or "
    r2 <- rangeP
    return $ FR field (\x -> r1 x || r2 x)

rangeP = do
    a <- int
    char '-'
    b <- int
    return $ \x -> x >= a && x <= b

int :: Parser Int
int = read <$> many1 digit

type Problem = ([FieldRule], Ticket, [Ticket])

ticketP = sepBy1 int $ char ','
inputP :: Parser Problem
inputP = do
    rules <- many1 (fieldRuleP <* endOfLine)
    endOfLine
    string "your ticket:"
    endOfLine
    ticket <- ticketP
    endOfLine
    endOfLine
    string "nearby tickets:"
    endOfLine
    tickets <- many1 (ticketP <* endOfLine)
    return (rules, ticket, tickets)

part1 :: Problem -> Int
part1 (rules, mine, nearby) = sum . filter (not . validForAny rules) . concat $ nearby
validForAny :: [FieldRule] -> Int -> Bool
validForAny rules n = any (`validates` n) rules
validates (FR _ f) n = f n

validTicket rules ticket = all (validForAny rules) ticket
fieldValues tickets = map extractCol [0..length (tickets!!0)-1]
    where extractCol n = map (!!n) tickets
validatesAll field values = all (field `validates`) values
possibleFields rules values = filter (`validatesAll` values) rules

part2 (rules, mine, nearby) = product departures where
    flexibility = length . possibleFields rules
    columns = fieldValues . filter (validTicket rules) $ nearby
    -- allocate the most constrained fields first
    scols = sortOn (flexibility . snd) $ zip [0..] columns
    departures = [ mine!!i |
        (i, FR x _) <- M.toList solution,
        "departure" `isPrefixOf` x]
    solution = head $ solve rules scols
    -- recursively solve for the list of all valid allocations
    solve :: [FieldRule]  -- fields yet to be allocated
        -> [(Int, [Int])] -- cols yet to be allocated
        -> [M.Map Int FieldRule]  -- possible field allocations
    solve [] [] = [M.empty]
    solve [] _ = error "Rule count mismatch"
    solve _ [] = error "Rule count mismatch"
    solve rs ((i,v):cs) = [
        M.insert i r s |
        (r, rr) <- removeEach rs,
        r `validatesAll` v,
        s <- solve rr cs]
    removeEach :: [a] -> [(a, [a])]
    removeEach [] = []
    removeEach (x:xs) = (x, xs):[(y, x:ys) | (y, ys) <- removeEach xs]

main = do
    input <- parseOrDie inputP <$> readFile "day16.txt"
    print $ part1 input
    print $ part2 input
