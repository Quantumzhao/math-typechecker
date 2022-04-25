{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Printer.FormatDef where
  
data Expr 
  = SetExpr {
    name :: String,
    tags :: [String],
    body :: SetBodyExpr,
    wheres :: WhereExpr
  }
  | MappingExpr {
    name :: String,
    tags :: [String],
    left :: SetBodyExpr,
    right :: SetBodyExpr,
    wheres :: WhereExpr
  }
  | RelExpr {
    left :: SetBodyExpr, 
    right :: SetBodyExpr,
    name :: String, 
    tags :: [String],
    forallLeft :: QualifierExpr,
    forallRight :: QualifierExpr,
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

data QualifierExpr 
  = BlankQualifier
  | ForAll String
  | Exist String String

data SetBodyExpr
  = SetTypeOfExpr String
  | SetContaining [String]

data WhereExpr 
  = Clause [Expr]
  | BlankWhere
