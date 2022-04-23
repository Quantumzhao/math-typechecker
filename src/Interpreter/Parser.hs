module Interpreter.Parser where

import Prelude hiding ((<>))
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Error
import Data.Void

type Parser = Parsec Void String

sb :: Parser a -> Parser b -> Parser c -> Parser c
sb p1 p2 p = space *> p1 *> space *> p <* space <* p2

parse :: String -> a
parse = undefined

parseCommand :: Parser String
parseCommand = string "" <|> string "more info " <|> string "exit"

parseDefEntry = undefined

parseMathDef = undefined

parseNaiveSetDef = undefined

parseOther = undefined
