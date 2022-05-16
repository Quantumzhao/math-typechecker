module Interpreter.Parser where

import Prelude hiding ((<>))
import Text.Megaparsec
  ( (<|>),
    empty,
    optional,
    anySingle,
    runParser,
    satisfy,
    choice,
    many,
    some,
    Parsec,
    MonadParsec(try) )
import Text.Megaparsec.Char ( char, space, string )
import Data.Void ( Void )
import Interpreter.AST
  ( MathExp(Tuple, Variable, Apply1, Apply2),
    Closure,
    Claim(Claim),
    Symbol(Symbol),
    Tuple(TupleDef),
    Object(ObjectDef),
    Relation(RelDef),
    Mapping(MappingDef),
    Class(ClassDef),
    MathDef(..),
    DefEntry(DefEntry),
    Command(..) )
import Data.Char ( isAlphaNum )
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad (void)
import Tags ( classTags, mappingTags, relationTags )

type Parser = Parsec Void String

-- all valid characters that can appear in a label
labelChars :: Parser Char
labelChars = satisfy $ \ c -> isAlphaNum c || c `elem` ['_', '-', '\'', '.']

parseValidTags :: [String] -> Parser String
parseValidTags tags = choice (string <$> tags) <* space

-- the entry point for parsing
parse :: String -> Either String Command
parse s = case runParser mainParse "" s of
  Right r -> return r
  Left l -> Left $ show l

-- the actual part that does stuff
mainParse :: Parser Command
mainParse = choice [
    parseClaim,
    try parseExit,
    try parseDef, 
    try expr2Command
  ]

-- definition -> command
parseDef :: Parser Command
parseDef = Definition <$> parseDefEntry

-- parse a multiline structure (presumably from the output of a file)
-- and then return a list of commands
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

-- parse a single definition entry
-- which should be in the form "? := ...\n"
parseDefEntry :: Parser DefEntry
parseDefEntry = do
  many (char '\n')
  sym <- some labelChars
  string " := "
  def <- parseMathDef
  -- this part hasn't been implemented yet, so leave it empty
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

-- a claim can be in two notations
-- a ~ b by ? or a ? b
parseClaim :: Parser Command
parseClaim = ClaimOf <$> (try parseClaimTilde <|> try parseClaimInfix)

-- a ~ b notation
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

-- a ? b notation
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
