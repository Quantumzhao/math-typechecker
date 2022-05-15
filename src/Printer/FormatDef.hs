{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Printer.FormatDef where
  
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
  | ClaimExpr

data QualifierExpr 
  = BlankQualifier
  | ForAll String
  | Exist String String

data WhereExpr 
  = Clause [Expr]
  | BlankWhere
