module Common where

import GHC.Natural
import GHC.Base (Symbol)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data BooleanExpression a
  = Not (BooleanExpression a) 
  | And (BooleanExpression a) (BooleanExpression a) 
  | Or (BooleanExpression a) (BooleanExpression a)
  | Xor (BooleanExpression a) (BooleanExpression a)
  | Characteristic a

data Finite = Size Natural | Range Natural Natural 

data Infinite = Countable | Uncountable | MoreThan Natural | LessThan

data Relation a b

type SymbolGenerator = Int

-- data BinaryOperation a b c = BinOp a b c Bijectivity Partiality

(>-) :: Natural -> Natural -> Natural
(>-) a b = if a <= b then 0 else a - b

