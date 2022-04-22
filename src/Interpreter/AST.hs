module Interpreter.AST where

data Command
  = Definition DefEntry
  | AnonymousExpr String
  | Info String
  | Exit

data DefEntry = DefEntry {
  symbol :: String,
  defBody :: MathDef, 
  wheres :: Closure
}

data MathDef
  = ToSet NaiveSetDef
  | ToOther OtherStructureDef

data NaiveSetDef 
  = SetDef {
    name :: String,
    setTags :: [String],
    body :: SetBodyDef
  }
  | SetVariable String

data OtherStructureDef
  = MappingDef {
    domain :: NaiveSetDef,
    range :: NaiveSetDef,
    tags :: [String]
  }
  | RelDef {
    from :: NaiveSetDef,
    to :: NaiveSetDef,
    tags :: [String]
  }
  | ObjectDef {
    set :: NaiveSetDef
  }
  | OtherVariable String

data Closure
  = Clause [DefEntry]
  | BlankClosure

data SetBodyDef
  = SetTypeDef
  | SetContainingDef

-- data 
