import AOC

part1 :: String -> Int
part1 = sum . map (parseOrDie calcP) . lines

calcP :: Parser Int
calcP = chainl1 (parenP <|> numP) opP

numP :: Parser Int
numP = read <$> many1 digit

opP :: Parser (Int -> Int -> Int)
opP = choice
    [ (*) <$ char '*'
    , (+) <$ char '+'
    , (-) <$ char '-'
    ]

parenthesized :: Parser a -> Parser a
parenthesized p = do
    char '('
    x <- p
    char ')'
    return x

parenP :: Parser Int
parenP = parenthesized calcP

part2 = sum . map (parseOrDie calc2) . lines

calc2 :: Parser Int
calc2 = chainl1 term ((*) <$ char '*')
term = chainl1 factor op2
    where op2 = ((+) <$ char '+') <|> ((-) <$ char '-')
factor = parenthesized calc2 <|> numP

main = do
    -- Handling spaces requires a bunch of backtracking, so we just strip them
    -- out at the start
    txt <- filter (/=' ') <$> readFile "day18.txt"
    print $ part1 txt
    print $ part2 txt
