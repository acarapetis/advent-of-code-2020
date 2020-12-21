import AOC
import Grid
import Data.List (intercalate, find)

type ImageTile = Grid Bool
showImage = showWith pixelChar where
    pixelChar False = '.'
    pixelChar True = '#'

parseTile :: Parser (Int, ImageTile)
parseTile = do
    string "Tile "
    n <- many1 digit
    string ":"
    endOfLine
    grid <- many1 (many1 parsePixel <* endOfLine)
    optional endOfLine
    return (read n, fromListOfLists grid)

parsePixel :: Parser Bool
parsePixel = (False <$ char '.') <|> (True <$ char '#')

fit :: ImageTile -- tile x to keep fixed
    -> ImageTile -- tile y to rotate
    -> Cardinal -- edge of x to fit along
    -> Int -- number of times to rotate y
    -> Bool
fit x y d n = edge x d == edge y' d' where
    y' = funcpow n rotateGrid y
    d' = reflect d

{-
findFit x ys = find (\(y, d, n) -> fit x y d n) [
    (y, d, n)
]
-}

main = do
    input <- parseOrDie (many1 parseTile) <$> readFile "day20.txt"
    putStrLn . intercalate "\n\n" . map (showImage.snd) $ input
