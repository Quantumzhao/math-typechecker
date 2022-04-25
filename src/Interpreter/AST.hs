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
  = ToCollection Collection
  | ToOther OtherStructureDef

data Collection 
  = CollectionDef {
    name :: String,
    collectionTags :: [String],
    body :: SetBodyDef
  }
  | CVariableRep'n String

data OtherStructureDef
  = MappingDef {
    domain :: Collection,
    range :: Collection,
    tags :: [String]
  }
  | RelDef {
    nameO :: String,
    from :: Collection,
    to :: Collection,
    tags :: [String]
  }
  | ObjectDef {
    nameO :: String,
    set :: Collection
  }
  | OtherVariable String

data Closure
  = Clause [DefEntry]
  | BlankClosure

data SetBodyDef
  = SetTypeDef Collection
  | SetContainingDef [MathDef]

-- data 
