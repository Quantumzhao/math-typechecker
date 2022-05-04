module Interpreter.Parser where

import Prelude hiding ((<>))
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Error
import Data.Void
import Interpreter.AST
import Control.Applicative.HT

type Parser = Parsec Void String

exprChars :: Parser Char
exprChars = char '(' <|> alphaNumChar <|> char ')'

sb :: Parser a -> Parser b -> Parser c -> Parser c
sb p1 p2 p = space *> p1 *> space *> p <* space <* p2

parse :: Parser Command
parse = parseDefEntry <|> parseExpr <|> parseInfo <|> parseExit

parseCommand :: Parser String
parseCommand = string "" <|> string "more info " <|> string "exit"

parseDefEntry :: Parser Command
parseDefEntry = undefined

parseExpr :: Parser Command
parseExpr = 
  let expr = parseVariable <|> parseApply1 <|> parseApply2 <|> parseTuple in
  lift2 AnonymousExpr (many exprChars) expr

parseVariable :: Parser MathExp
parseVariable = undefined

parseApply1 :: Parser MathExp
parseApply1 = undefined

parseApply2 :: Parser MathExp
parseApply2 = undefined

parseTuple :: Parser MathExp
parseTuple = undefined

parseInfo :: Parser Command
parseInfo = undefined

parseExit :: Parser Command
parseExit = Exit <$ string "Exit"
