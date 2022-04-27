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
  = ToClass Class
  | ToOther OtherStructureDef

data Class 
  = ClassDef {
    name :: String,
    classTags :: [String]
  }
  | CVariableRep'n String

data OtherStructureDef
  = MappingDef {
    domain :: Class,
    range :: Class,
    tags :: [String]
  }
  | RelDef {
    nameO :: String,
    from :: Class,
    to :: Class,
    tags :: [String]
  }
  | ObjectDef {
    nameO :: String,
    set :: Class
  }
  | OtherVariable String

data Closure
  = Clause [DefEntry]
  | BlankClosure

data SetBodyDef
  = SetTypeDef Class

-- data 
