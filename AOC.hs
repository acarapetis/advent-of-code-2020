module AOC (module AOC, module EXPORT_ALL, (<*)) where

import Text.Parsec as EXPORT_ALL hiding (parse)
import qualified Text.Parsec
import Control.Applicative ((<*))
type Parser = Parsec String ()

parse :: Parser t -> String -> Either ParseError t
parse p = Text.Parsec.parse p ""

parseOrDie :: Parser t -> String -> t
parseOrDie p s = case parse p s of 
    Left x -> error $ show x
    Right x -> x
