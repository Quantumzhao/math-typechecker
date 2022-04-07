module Common where

import GHC.Natural

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data BooleanExpression = Not | And | Or | Xor

data Cardinal = Size Natural | CountableInf | UncountableInf

data Relation a b

-- data BinaryOperation a b c = BinOp a b c Bijectivity Partiality

