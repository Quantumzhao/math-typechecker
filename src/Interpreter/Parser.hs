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

-- >>> runParser mainParse "" "claim: A isSubsetOf A"
-- Right (ClaimOf (Claim {fromC = Variable (Symbol {reference = "A"}), toC = Variable (Symbol {reference = "A"}), relation = Variable (Symbol {reference = "isSubsetOf"})}))

-- >>> runParser parseClaim "" "claim: A ~ B by isSubsetOf"
-- Right (ClaimOf (Claim {fromC = Variable (Symbol {reference = "A"}), toC = Variable (Symbol {reference = "B"}), relation = Variable (Symbol {reference = "isSubsetOf"})}))

labelChars :: Parser Char
labelChars = satisfy $ \ c -> isAlphaNum c || c `elem` ['_', '-', '\'']

reservedName :: [String]
reservedName = ["Set", "Class", "Relation", "Mapping"]

parseValidTags :: [String] -> Parser String
parseValidTags tags = choice (string <$> tags) <* space

sb :: Parser a -> Parser b -> Parser c -> Parser c
sb p1 p2 p = space *> p1 *> space *> p <* space <* p2

parse :: String -> Either String Command
parse s = case runParser mainParse "" s of
  Right r -> return r
  Left l -> Left $ show l

mainParse :: Parser Command
mainParse = choice [
    parseClaim,
    try parseExit,
    try parseDef, 
    try expr2Command
  ]

parseDef :: Parser Command
parseDef = Definition <$> parseDefEntry

getAllParsed :: String -> Either String [Command]
getAllParsed contents = 
  let main = do
            ds <- some mainParse
            optional anySingle
            pure ds
  in
  let parsed = runParser main "" contents in
  case parsed of
    Right r -> return r
    Left l -> Left $ show l

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
  range <- parseExpr
  optional space
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
  optional space
  pure $ FromRelationAST $ RelDef from to tags

parseObjDef :: Parser MathDef
parseObjDef = string "element in " *> (FromObjectAST . ObjectDef <$> parseExpr) <* optional space

parseTupleDef :: Parser MathDef
parseTupleDef = do
  char '('
  left <- parseExpr
  string ", "
  right <- parseExpr
  char ')'
  pure $ FromTupleAST $ TupleDef left right

parseClaim :: Parser Command
parseClaim = ClaimOf <$> (try parseClaimTilde <|> try parseClaimInfix)

parseClaimTilde :: Parser Claim
parseClaimTilde = do
  string "claim: "
  left <- parseExpr
  string " ~ "
  right <- parseExpr
  string " by "
  rel <- parseExpr
  let res = Claim left right rel
  pure res

parseClaimInfix :: Parser Claim
parseClaimInfix = do
  string "claim: "
  left <- parseExpr
  char ' '
  rel <- parseExpr
  char ' '
  right <- parseExpr
  let res = Claim left right rel
  pure res

parseWhere :: Parser Closure
parseWhere = pure []

expr2Command :: Parser Command
expr2Command = AnonymousExpr <$> parseExpr

parseExpr :: Parser MathExp
parseExpr = try parseApply2 <|> try parseApply1 <|> try parseTuple <|> try (Variable <$> parseSymbol)

parseSymbol :: Parser Symbol
parseSymbol = Symbol <$> some labelChars

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
