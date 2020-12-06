import Text.Parsec
import qualified Data.Set as S
import Control.Applicative ((<*))

part1 = show . sum . map groupScore . parseGroups
groupScore = S.size . S.unions

person = fmap S.fromList $ many1 letter
group = many1 (person <* endOfLine)
groups = sepBy1 group endOfLine

parseGroups = _pg . parse groups "" where
    _pg (Left x) = error $ show x
    _pg (Right x) = x
    
groupScore2 = S.size . foldl1 S.intersection
part2 = show . sum . map groupScore2 . parseGroups

main = do
    input <- readFile "day6.txt"
    putStrLn $ part1 input
    putStrLn $ part2 input
