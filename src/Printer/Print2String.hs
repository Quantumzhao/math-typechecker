module Printer.Print2String where

import Prelude hiding (print)
import Node
import Set
import ContextState
import Printer.FormatDef as P
import Data.List (intercalate)

printExpr :: Expr -> [String]
printExpr (SetExpr name tags wheres) =
  if name == "Universal" || name == "Empty" then [name]
  else (name ++ " is " ++ unwords tags ++ " " ++ name)
  `combineWith` wheres

printExpr (MappingExpr name tags left right wheres) =
  let tagsStr = if null tags then "" else ", -> is " ++ unwords tags ++ ", "
      main = name ++ " is " ++ left ++ " -> " ++ right ++ tagsStr
  in main `combineWith` wheres

printExpr (RelExpr left right by tags wheres) =
  let main = left ++ " is related to " ++ right ++
             " by " ++ by ++ intercalate ", " tags
  in main `combineWith` wheres

printExpr (ObjectExpr name set wheres) = (name ++ " is element in " ++ set) `combineWith` wheres

printExpr (TupleExpr name l r wheres) = (name ++ " is (" ++ l ++ ", " ++ r ++ ")") `combineWith` wheres

printWhereExpr :: WhereExpr -> [String]
printWhereExpr (Clause exprs) = " where" : printWhereExpr' exprs
  where
    printWhereExpr' [] = []
    printWhereExpr' (x : xs) = ("  " ++) <$> (printExpr x ++ printWhereExpr' xs)
printWhereExpr BlankWhere = []

printQualifierExpr :: QualifierExpr -> String
printQualifierExpr BlankQualifier = ""
printQualifierExpr (P.ForAll set) = ", for all in " ++ set
printQualifierExpr (P.Exist symbol set) = symbol ++ " is in " ++ set

combineWith :: String -> WhereExpr -> [String]
combineWith main wheres =
  case printWhereExpr wheres of
  x : xs -> (main ++ x) : xs
  [] -> [main]
