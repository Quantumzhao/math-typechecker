module Printer.Print2String where

import Node
import Set
import ContextState
import Printer.FormatDef

print :: Expr -> [String]
print (SetExpr qualifier body wheres) = undefined 
print (MappingExpr qualifier left right wheres) = undefined 
print (RelExpr left right by forallLeft forallRight wheres) = undefined
print (ObjectEXpr object set wheres) = (object ++ " is element in " ++ set) `combineWith` wheres
print (TupleExpr left right wheres) = undefined 

printWhereExpr :: WhereExpr -> [String]
printWhereExpr (Clause exprs) = undefined 
printWhereExpr BlankWhere = []

printQualifierExpr :: QualifierExpr -> String
printQualifierExpr BlankQualifier = ""
printQualifierExpr (ForAll symbol set) = "for all " ++ symbol ++ " in " ++ set ++ ", "
printQualifierExpr (Exist symbol set) = "exist " ++ symbol ++ " in " ++ set ++ ", " 

printSetBodyExpr :: SetBodyExpr -> String
printSetBodyExpr (SetTypeOfExpr setType) = undefined 
printSetBodyExpr (SetContaining es) = undefined 

combineWith :: String -> WhereExpr -> [String]
combineWith main wheres =
  case printWhereExpr wheres of
  x : xs -> (main ++ x) : xs
  [] -> [main]
