module AOC (module AOC, module EXPORT_ALL, (<*)) where

import Text.Parsec as EXPORT_ALL hiding (parse, uncons)
import qualified Text.Parsec
import Control.Applicative ((<*))
import Data.Maybe as EXPORT_ALL
import Data.List as EXPORT_ALL

type Parser = Parsec String ()

parse :: Parser t -> String -> Either ParseError t
parse p = Text.Parsec.parse p ""

parseOrDie :: Parser t -> String -> t
parseOrDie p s = case parse p s of 
    Left x -> error $ show x
    Right x -> x

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust ((Just x):_) = Just x
firstJust (Nothing:xs) = firstJust xs

readNums :: String -> [Int]
readNums = map read . lines

funcpow :: Int -> (a -> a) -> a -> a
funcpow n f
    | n == 0     = id
    | n >= 1     = f . funcpow (n-1) f
    | otherwise  = error "n must be positive"
