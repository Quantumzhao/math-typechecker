module Interpreter.Parser where

import Prelude hiding ((<>))
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Error
import Data.Void
import Interpreter.AST
import Control.Applicative.HT
import Data.Char

type Parser = Parsec Void String

-- >>> runParser parseApply1 "" "f(a)"
-- Left (ParseErrorBundle {bundleErrors = TrivialError 3 (Just (Tokens (')' :| ""))) (fromList [Tokens ('(' :| "")]) :| [], bundlePosState = PosState {pstateInput = "f(a)", pstateOffset = 0, pstateSourcePos = SourcePos {sourceName = "", sourceLine = Pos 1, sourceColumn = Pos 1}, pstateTabWidth = Pos 8, pstateLinePrefix = ""}})

-- >>> runParser parseVariable "" "f"
-- Right (Variable (Symbol {reference = "f"}))

exprChars :: Parser Char
exprChars = char '(' <|> labelChars <|> char ')'

labelChars :: Parser Char
labelChars = satisfy $ \ c -> isAlphaNum c || c `elem` ['_', '-', '\'']

sb :: Parser a -> Parser b -> Parser c -> Parser c
sb p1 p2 p = space *> p1 *> space *> p <* space <* p2

parse :: String -> Command
parse s = case runParser main "" s of
  Right r -> r
  Left l -> error $ show l
  where main = parseDefEntry <|> expr2Command <|> parseInfo <|> parseExit

parseCommand :: Parser String
parseCommand = string "" <|> string "more info " <|> string "exit"

parseDefEntry :: Parser Command
parseDefEntry = undefined

expr2Command :: Parser Command
expr2Command = fmap AnonymousExpr parseExpr

parseExpr :: Parser MathExp
parseExpr = parseApply2 <|> parseApply1 <|> parseTuple <|> parseVariable

parseVariable :: Parser MathExp
parseVariable = Variable <$> parseSymbol

parseSymbol :: Parser Symbol
parseSymbol = Symbol <$> some labelChars

parseApply1 :: Parser MathExp
parseApply1 = lift2 Apply1 parseSymbol (between (parseSymbol *> char '(') (char ')') parseExpr)

parseApply2 :: Parser MathExp
parseApply2 = do
  f <- parseSymbol
  char '('
  e1 <- parseExpr
  string ", "
  e2 <- parseExpr
  char ')'
  pure $ Apply2 f e1 e2

parseTuple :: Parser MathExp
parseTuple = do
  char '('
  e1 <- parseExpr
  string ", "
  e2 <- parseExpr
  char ')'
  pure $ Tuple e1 e2

parseInfo :: Parser Command
parseInfo = undefined

parseExit :: Parser Command
parseExit = Exit <$ string "Exit"
