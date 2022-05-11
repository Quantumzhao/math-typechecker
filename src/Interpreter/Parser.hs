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
import Tags

type Parser = Parsec Void String

-- >>> parseDef "A := finite Set"
-- Couldn't match expected type ‘[Char] -> t’
--             with actual type ‘ParsecT Void String Identity Command’

-- >>> runParser parseDef "" "A := finite Set"
-- Right (Definition (DefEntry {symbol = "A", defBody = FromClassAST (ClassDef {tagsC = ["finite","Set"]}), wheres = []}))

-- >>> runParser parseMapDef "" "injective Mapping A -> B"
-- Right (FromMappingAST (MappingDef {domainM = Symbol {reference = "A"}, rangeM = Symbol {reference = "B"}, tagsM = ["injective"]}))

labelChars :: Parser Char
labelChars = satisfy $ \ c -> isAlphaNum c || c `elem` ['_', '-', '\'']

reservedName :: [String]
reservedName = ["Set", "Class", "Relation", "Mapping"]

parseValidTags :: [String] -> Parser String
parseValidTags tags = choice (string <$> tags) <* space

sb :: Parser a -> Parser b -> Parser c -> Parser c
sb p1 p2 p = space *> p1 *> space *> p <* space <* p2

parse :: String -> Command
parse s = case runParser main "" s of
  Right r -> r
  Left l -> error $ show l
  where 
    main = choice [
        try parseExit,
        try parseDef, 
        try expr2Command
      ]

parseDef :: Parser Command
parseDef = Definition <$> parseDefEntry

getParsedDefs :: String -> [DefEntry]
getParsedDefs contents = 
  let main = do
            ds <- some parseDefEntry
            optional anySingle
            pure ds
  in
  let parsed = runParser main "" contents in
  case parsed of
    Right r -> r
    Left l -> error $ show l

parseDefEntry :: Parser DefEntry
parseDefEntry = do
  many (char '\n')
  sym <- some labelChars
  string " := "
  def <- parseMathDef
  optional $ string "where"
  w <- parseWhere <|> ([] <$ empty)
  let res = DefEntry sym def w
  pure res

parseMathDef :: Parser MathDef
parseMathDef = choice [
    try parseMapDef,
    try parseClassDef,
    try parseRelDef,
    try parseObjDef,
    try parseTupleDef,
    FromExpr <$> parseExpr
  ]

parseMapDef :: Parser MathDef
parseMapDef = do
  tags <- many (parseValidTags mappingTags) <* string "Mapping "
  domain <- parseExpr
  string " -> "
  -- error $ show domain
  range <- parseExpr
  pure $ FromMappingAST $ MappingDef domain range tags

parseClassDef :: Parser MathDef
parseClassDef = do
  tags <- many (parseValidTags classTags)
  next <- string "Set" <|> string "Class"
  optional space
  let tags' = if next == "Set" then tags ++ ["Set"] else tags
  pure $ FromClassAST $ ClassDef tags'

parseRelDef :: Parser MathDef
parseRelDef = do
  tags <- many (parseValidTags relationTags) <* string "Relation between "
  from <- parseExpr
  string " and "
  to <- parseExpr
  pure $ FromRelationAST $ RelDef from to tags

parseObjDef :: Parser MathDef
parseObjDef = string "element in " *> (FromObjectAST . ObjectDef <$> parseExpr)

parseTupleDef :: Parser MathDef
parseTupleDef = do
  char '('
  left <- parseExpr
  string ", "
  right <- parseExpr
  char ')'
  pure $ FromTupleAST $ TupleDef left right

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
