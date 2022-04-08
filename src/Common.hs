module Common where

import GHC.Natural

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data BooleanExpression = Not | And | Or | Xor

data Finite = Size Natural | Range Natural Natural 

data Infinite = Countable | Uncountable | MoreThan Natural | LessThan

data Relation a b

-- data BinaryOperation a b c = BinOp a b c Bijectivity Partiality

