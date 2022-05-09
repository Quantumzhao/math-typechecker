{-# LANGUAGE TupleSections #-}
module Interpreter.Parser where

import Prelude hiding ((<>))
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import Data.Void
import Interpreter.AST
import Data.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad (void)

type Parser = Parsec Void String

-- >>> parseDef "A := finite Set"
-- Couldn't match expected type ‘[Char] -> t’
--             with actual type ‘ParsecT Void String Identity Command’

-- >>> runParser parseDef "" "f := A -> B"
-- Right (Definition (DefEntry {symbol = "f", defBody = FromMappingAST (MappingDef {domainM = Symbol {reference = "A"}, rangeM = Symbol {reference = "B"}, tagsM = []}), wheres = []}))

exprChars :: Parser Char
exprChars = char '(' <|> labelChars <|> char ')'

labelChars :: Parser Char
labelChars = satisfy $ \ c -> isAlphaNum c || c `elem` ['_', '-', '\'']

reservedName :: [String]
reservedName = ["Set", "Class", "Relation", "Mapping"]

sb :: Parser a -> Parser b -> Parser c -> Parser c
sb p1 p2 p = space *> p1 *> space *> p <* space <* p2

parse :: String -> Command
parse s = case runParser main "" s of
  Right r -> r
  Left l -> error $ show l
  where main = try parseExit <|> try parseDef <|> try expr2Command <|> try parseInfo

parseDef :: Parser Command
parseDef = Definition <$> parseDefEntry

parseTags :: Parser [String]
parseTags = undefined

getParsedDefs :: String -> [DefEntry]
getParsedDefs contents = 
  let parser = runParser (some parseDefEntry) "" contents in
  case parser of
    Right r -> r
    Left l -> error $ show l

parseDefEntry :: Parser DefEntry
parseDefEntry = do
  many (char '\n')
  sym <- some labelChars
  string " := "
  def <- try parseMapDef <|>
         try parseClassDef <|>
         try parseRelDef <|>
         try parseObjDef <|>
         try parseTupleDef <|>
         FromSymbol <$> parseSymbol
  optional $ string "where"
  w <- parseWhere <|>([] <$ empty)
  let res = DefEntry sym def w
  pure res

parseMapDef :: Parser MathDef
parseMapDef = do
  domain <- parseSymbol
  string " -> "
  -- error $ show domain
  range <- parseSymbol
  pure $ FromMappingAST $ MappingDef domain range []

parseClassDef :: Parser MathDef
parseClassDef = do
  tags <- some (some labelChars <* optional (char ' '))
  let tags' = case last tags of
        "Set" -> tags
        "Class" -> init tags
        s -> error $ "parseClassDef: " ++ s
  pure $ FromClassAST $ ClassDef tags'

parseRelDef :: Parser MathDef
parseRelDef = undefined

parseObjDef :: Parser MathDef
parseObjDef = undefined

parseTupleDef :: Parser MathDef
parseTupleDef = undefined

parseWhere :: Parser Closure
parseWhere = pure []

expr2Command :: Parser Command
expr2Command = AnonymousExpr <$> parseExpr

parseExpr :: Parser MathExp
parseExpr = try parseApply2 <|> try parseApply1 <|> try parseTuple <|> try (Variable <$> parseSymbol)

parseSymbol :: Parser Symbol
parseSymbol = Symbol <$> some labelChars

-- no leading space, one or more trailing space
-- parseTags :: Parser [String]
-- parseTags =
--   let validTag = some (satisfy (\ (c :: Char) -> not False)) in
--   many (some labelChars <* space)

parseApply1 :: Parser MathExp
parseApply1 = do
  f <- parseSymbol
  char '('
  e <- parseExpr
  char ')'
  pure $ Apply1 f e

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
parseExit = Exit <$ string "exit"



-- >>> runParser (pItemList <* eof) "" "something\n  one"
-- Right ("something",[("one",[])])

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

scn :: Parser ()
scn = L.space space1 lineComment empty

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

pItem :: Parser String
pItem = lexeme (some (alphaNumChar <|> char '-')) <?> "list item"

pComplexItem :: Parser (String, [String])
pComplexItem = L.indentBlock scn p
  where
    p = do
      header <- pItem
      return (L.IndentMany Nothing (return . (header, )) pItem)

pItemList :: Parser (String, [(String, [String])])
pItemList = L.nonIndented scn (L.indentBlock scn p)
  where
    p = do
      header <- pItem
      return (L.IndentSome Nothing (return . (header, )) pComplexItem)
