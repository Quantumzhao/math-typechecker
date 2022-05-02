module Interpreter.AST where

-- a single command that users can input
data Command
  -- defining a structure
  = Definition DefEntry
  | AnonymousExpr String MathExp
  -- show more information on a structure, 
  -- needs to be reworked later
  | Info String
  -- meet its inevitable demise
  | Exit

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
}

data MathDef
  = FromClassAST Class
  | FromMappingAST Mapping
  | FromRelationAST Relation
  | FromObjectAST Object
  | FromTupleAST Tuple
  | FromSymbol Symbol

data Class = ClassDef {
  tagsC :: [String]
}

data Mapping = MappingDef {
  domainM :: Symbol,
  rangeM :: Symbol,
  tagsM :: [String]
}

data Relation = RelDef {
  fromR :: Symbol,
  toR :: Symbol,
  tagsR :: [String]
}

data Object = ObjectDef {
  setO :: Symbol
}

data Tuple = TupleDef {
  leftT :: Symbol,
  rightT :: Symbol,
  tags :: [String]
}

data Symbol = Symbol {
  referFrom :: String
}

type Closure = [(Bool, DefEntry)]

data MathExp
  = Apply1 Symbol  MathExp
  | Apply2 Symbol MathExp MathExp
  | Relate Symbol MathExp MathExp
  | Tuple MathExp MathExp
  | Variable Symbol
