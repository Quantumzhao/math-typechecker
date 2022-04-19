{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Printer.FormatDef where
  
data Expr 
  = SetExpr {
    qualifier :: QualifierExpr,
    body :: SetBodyExpr,
    wheres :: WhereExpr
  }
  | MappingExpr {
    qualifier :: QualifierExpr,
    left :: String,
    right :: String,
    wheres :: WhereExpr
  }
  | RelExpr {
    left :: String, 
    right :: String,
    by :: String, 
    forallLeft :: QualifierExpr,
    forallRight :: QualifierExpr,
    wheres :: WhereExpr
  }
  | ObjectEXpr {
    object :: String,
    set :: String,
    wheres :: WhereExpr
  }
  | TupleExpr {
    left :: String,
    right :: String,
    wheres :: WhereExpr
  }

data QualifierExpr 
  = BlankQualifier
  | ForAll String String
  | Exist String String

data SetBodyExpr
  = SetTypeOfExpr String
  | SetContaining [String]

data WhereExpr 
  = Clause [Expr]
  | BlankWhere
