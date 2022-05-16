module Printer.FormatDef where
  
-- the definitions for the output expression
data Expr 
  = SetExpr {
    name :: String,
    tags :: [String],
    wheres :: WhereExpr
  }
  | MappingExpr {
    name :: String,
    tags :: [String],
    left :: String,
    right :: String,
    wheres :: WhereExpr
  }
  | RelExpr {
    left :: String, 
    right :: String,
    name :: String, 
    tags :: [String],
    wheres :: WhereExpr
  }
  | ObjectExpr {
    name :: String,
    set :: String,
    wheres :: WhereExpr
  }
  | TupleExpr {
    name :: String,
    first :: String,
    second :: String,
    wheres :: WhereExpr
  }
  -- claim does not have any information, because it is not a value
  | ClaimExpr

data QualifierExpr 
  = BlankQualifier
  | ForAll String
  | Exist String String

data WhereExpr 
  = Clause [Expr]
  | BlankWhere
