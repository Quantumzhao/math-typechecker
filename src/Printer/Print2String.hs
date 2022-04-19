module Printer.Print2String where

import Prelude hiding (print)
import Node
import Set
import ContextState
import Printer.FormatDef
import Data.List (intercalate)

print :: Expr -> [String]
print (SetExpr symbol name tags body wheres) = 
  (symbol ++ " is " ++ intercalate ", " tags ++ " Set " ++ name ++ printSetBodyExpr body) 
  `combineWith` wheres
print (MappingExpr name tags left right qualifier wheres) =
  let main = name ++ " is " ++ printSetBodyExpr left ++ " -> " ++ printSetBodyExpr right ++
             ", -> is " ++ intercalate ", " tags ++ ", " 
  in main `combineWith` wheres
print (RelExpr left right by tags forallLeft forallRight wheres) =
  let main = printSetBodyExpr left ++ " is related to " ++ printSetBodyExpr right ++ 
             " by " ++ by ++ intercalate ", " tags ++ 
             printQualifierExpr forallLeft ++ printQualifierExpr forallRight
  in main `combineWith` wheres
print (ObjectExpr name set wheres) = (name ++ " is element in " ++ set) `combineWith` wheres
print (TupleExpr name l r wheres) = (name ++ " is (" ++ l ++ ", " ++ r ++ ")") `combineWith` wheres

printWhereExpr :: WhereExpr -> [String]
printWhereExpr (Clause exprs) = " where" : printWhereExpr' exprs
  where
    printWhereExpr' [] = []
    printWhereExpr' (x : xs) = ("  " ++) <$> (print x ++ printWhereExpr' xs)
printWhereExpr BlankWhere = []

printQualifierExpr :: QualifierExpr -> String
printQualifierExpr BlankQualifier = ""
printQualifierExpr (ForAll symbol set) = ", for all " ++ symbol ++ " in " ++ set
printQualifierExpr (Exist symbol set) = symbol ++ " is in " ++ set

printSetBodyExpr :: SetBodyExpr -> String
printSetBodyExpr (SetTypeOfExpr setType) = "type of " ++ setType
printSetBodyExpr (SetContaining es) = "consisting of " ++ intercalate ", " es

combineWith :: String -> WhereExpr -> [String]
combineWith main wheres =
  case printWhereExpr wheres of
  x : xs -> (main ++ x) : xs
  [] -> [main]
