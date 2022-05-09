{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Interpreter.AST where

-- a single command that users can input
data Command
  -- defining a structure
  = Definition DefEntry
  | AnonymousExpr MathExp
  -- show more information on a structure, 
  -- needs to be reworked later
  | Info String
  -- meet its inevitable demise
  | Exit
  deriving (Show)

-- a single entry of the definition of a structure
-- in the form ? := ?
-- or ? ~ ?
-- or whatever
data DefEntry = DefEntry {
  -- the name of the variable
  symbol :: String,
  -- actual definition
  defBody :: MathDef, 
  -- everything in the wheres clause
  wheres :: Closure
} deriving (Show)

data MathDef
  = FromClassAST Class
  | FromMappingAST Mapping
  | FromRelationAST Relation
  | FromObjectAST Object
  | FromTupleAST Tuple
  | FromSymbol Symbol
  deriving (Show)

data Class = ClassDef {
  tagsC :: [String]
} deriving (Show)

data Mapping = MappingDef {
  domainM :: Symbol,
  rangeM :: Symbol,
  tagsM :: [String]
} deriving (Show)

data Relation = RelDef {
  fromR :: Symbol,
  toR :: Symbol,
  tagsR :: [String]
} deriving (Show)

data Object = ObjectDef {
  setO :: Symbol
} deriving (Show)

data Tuple = TupleDef {
  leftT :: Symbol,
  rightT :: Symbol,
  tags :: [String]
} deriving (Show)

data Symbol = Symbol {
  reference :: String
} deriving (Show)

type Closure = [(Bool, DefEntry)]

data MathExp
  = Apply1 Symbol MathExp
  | Apply2 Symbol MathExp MathExp
  | Relate Symbol MathExp MathExp
  | Tuple MathExp MathExp
  | Variable Symbol
  deriving (Show)
