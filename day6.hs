import qualified Data.Set as S
import Control.Applicative ((<*))
import Data.Set (member)
import Data.Either (fromRight)
import Text.Parsec

part1 = show . sum . map groupScore . parseGroups'
groupScore = S.size . S.unions

person = many1 letter
group = many1 (person <* endOfLine)
groups = sepBy1 group endOfLine

parseGroups = parse groups ""
parseGroups' = map (map S.fromList) . _pg . parseGroups where
    _pg (Left x) = error $ show x
    _pg (Right x) = x

    
groupScore2 = S.size . foldl1 S.intersection
part2 = show . sum . map groupScore2 . parseGroups'

main = do
    input <- readFile "day6.txt"
    putStrLn $ part1 input
    putStrLn $ part2 input
