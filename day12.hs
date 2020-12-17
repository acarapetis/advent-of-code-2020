import AOC
import Grid
data Ship = Ship Coord Cardinal deriving (Show, Eq)
data Cardinal = N | E | S | W deriving (Show, Eq)
data Action = Move Cardinal Int | Forward Int | Rotate Int deriving (Show, Eq)

dir N = (-1,0)
dir S = (1,0)
dir E = (0,1)
dir W = (0,-1)

applyCard c n p = addC p . scaleC n $ dir c

apply (Move d n) (Ship p z) = Ship (applyCard d n p) z
apply (Forward n) (Ship p z) = Ship (applyCard z n p) z
apply (Rotate n) (Ship p z) = Ship p (rotate n z)

reflect N = S
reflect S = N
reflect W = E
reflect E = W

rotate :: Int -> Cardinal -> Cardinal
rotate n x
    | n < 0 = reflect . rotate (-n) . reflect $ x
    | otherwise = r (n `div` 90) x where
        r 0 x = x
        r 1 N = W
        r 1 W = S
        r 1 S = E
        r 1 E = N
        r n x = r 1 $ r (n-1) x

actionP :: Parser Action
actionP = choice [moveP, forwardP, rotateP]

moveP = do
    d <- cardinalP
    n <- intP
    return $ Move d n

cardinalP = choice
    [ N <$ char 'N'
    , E <$ char 'E'
    , S <$ char 'S'
    , W <$ char 'W'
    ]

intP :: Parser Int
intP = read <$> many1 digit

forwardP = do
    char 'F'
    n <- intP
    return $ Forward n

rotateP = rotateL <|> rotateR
rotateL = do
    char 'L'
    n <- intP
    return $ Rotate n
rotateR = do
    char 'R'
    n <- intP
    return $ Rotate (-n)

parseMoves = map (parseOrDie actionP) . lines
part1 moves = manhattan pos
    where Ship pos _ = foldl (flip apply) (Ship (0,0) E) moves


data WayShip = WayShip Coord Coord deriving (Show, Eq)

apply' (Move d n) (WayShip p w) = WayShip p (applyCard d n w)
apply' (Rotate n) (WayShip p w) = WayShip p (rotate' n w)
apply' (Forward n) (WayShip p w) = WayShip newPos w
    where newPos = addC p $ scaleC n w

reflect' (y, x) = (-y, x)
rotate' n p
    | n < 0     = reflect' . rotate' (-n) . reflect' $ p
    | otherwise = r (n `div` 90) p where
        r 0 p = p
        r 1 (y, x) = (-x, y)
        r n p = r 1 $ r (n-1) p

part2 moves = manhattan pos
    where WayShip pos _ = foldl (flip apply') (WayShip (0,0) (-1, 10)) moves

main = do
    moves <- parseMoves <$> readFile "day12.txt"
    print $ part1 moves
    print $ part2 moves
