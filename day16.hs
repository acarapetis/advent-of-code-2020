import AOC

type FieldRule = (String, Int -> Bool)
type Ticket = [Int]

fieldRuleP :: Parser FieldRule
fieldRuleP = do
    field <- many1 (letter <|> char ' ')
    string ": "
    r1 <- rangeP
    string " or "
    r2 <- rangeP
    return (field, \x -> r1 x || r2 x)

rangeP = do
    a <- int
    char '-'
    b <- int
    return $ \x -> x >= a && x <= b

int :: Parser Int
int = read <$> many1 digit

ticketP = sepBy1 int $ char ','
inputP :: Parser ([FieldRule], Ticket, [Ticket])
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

part1 :: ([FieldRule], Ticket, [Ticket]) -> Int
part1 (rules, mine, nearby) = sum . filter (not . validForAny rules) . concat $ nearby
validForAny :: [FieldRule] -> Int -> Bool
validForAny rules n = any (`validates` n) rules
validates (_, f) n = f n

validTicket rules ticket = all (validForAny rules) ticket
fieldValues tickets = map extractCol [0..length (tickets!!0)-1]
    where extractCol n = map (!!n) tickets
possibleFields rules values = filter validatesAll rules
    where validatesAll field = all (field `validates`) values

part2 (rules, mine, nearby) = map (length . possibleFields rules)
    . fieldValues . filter (validTicket rules) $ nearby

main = do
    input <- parseOrDie inputP <$> readFile "day16.txt"
    print $ part1 input
    print $ part2 input
