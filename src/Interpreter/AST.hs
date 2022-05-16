{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Interpreter.AST where

-- a single command that users can input
-- or it can also be an entry in a definition file
data Command
  -- defining a structure
  = Definition DefEntry
  -- a claim that a relation can exist
  | ClaimOf Claim
  -- just an expression like f(x)
  | AnonymousExpr MathExp
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
  | FromExpr MathExp
  deriving (Show)

-- the class
-- can be promoted to a set if the tag is set to "Set"
data Class = ClassDef {
  tagsC :: [String]
} deriving (Show)

-- mapping/function/operator/transformation/permutation (how many names does it have???)
-- set the domain to a tuple to make it a binary operation
data Mapping = MappingDef {
  domainM :: MathExp,
  rangeM :: MathExp,
  tagsM :: [String]
} deriving (Show)

data Relation = RelDef {
  fromR :: MathExp,
  toR :: MathExp,
  tagsR :: [String]
} deriving (Show)

-- the most general type of mathematical objects
-- it can be anything, if no other information is given
data Object = ObjectDef {
  setO :: MathExp
} deriving (Show)

-- the cartesian product
data Tuple = TupleDef {
  leftT :: MathExp,
  rightT :: MathExp
} deriving (Show)

-- the name of a variable
data Symbol = Symbol {
  reference :: String
} deriving (Show)

-- a claim that an ordered pair (a, b) is in A * B
data Claim = Claim {
  fromC :: MathExp,
  toC :: MathExp,
  relation :: MathExp
} deriving (Show)

-- the variables captured in scope
type Closure = [(Bool, DefEntry)]

data MathExp
  -- f(?)
  = Apply1 Symbol MathExp
  -- f(?, ?)
  | Apply2 Symbol MathExp MathExp
  | Relate Symbol MathExp MathExp
  | Tuple MathExp MathExp
  | Variable Symbol
  deriving (Show)
