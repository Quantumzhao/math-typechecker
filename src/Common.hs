module Common where

import GHC.Natural
import GHC.Base (Symbol)
import Control.Monad.State.Lazy
import qualified Data.List as List

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data BooleanExpression a
  = Not (BooleanExpression a) 
  | And (BooleanExpression a) (BooleanExpression a) 
  | Or (BooleanExpression a) (BooleanExpression a)
  | Xor (BooleanExpression a) (BooleanExpression a)
  | ExpFalse
  | ExpTrue
  -- it will evaluate the qualifier to either true or false
  | Characteristic a
  deriving (Eq, Show)

data Finite = Size Natural | Range Natural Natural 

data Infinite = Countable | Uncountable | MoreThan Natural | LessThan

data Relation a b

-- data BinaryOperation a b c = BinOp a b c Bijectivity Partiality

(>-) :: Natural -> Natural -> Natural
(>-) a b = if a <= b then 0 else a - b

type Labels = [(String, Int)]
genLabel :: String -> State Labels String
genLabel base = do
  labels <- get
  case takeOut (\t -> fst t == base) labels of
    (Just (name, count), labels') -> do
      put $ (name, count + 1) : labels'
      return $ name ++ show count
    (Nothing, _) -> do 
      put $ (base, 0) : labels
      return ""
  where
    takeOut f (x : xs) =
      if f x then (Just x, xs)
      else 
        case takeOut f xs of
          (Nothing, xs') -> (Nothing, x : xs')
          (Just res, xs') -> (Just res, x : xs')
    takeOut _ [] = (Nothing, [])

substitute :: (Eq t) => BooleanExpression t -> t -> t -> BooleanExpression t
substitute ExpFalse fromVar toVar = ExpFalse
substitute ExpTrue fromVar toVar = ExpTrue
substitute (Characteristic c) fromVar toVar
  | c == fromVar = Characteristic toVar
  | otherwise = Characteristic c
substitute (Not inner) fromVar toVar = 
  Not $ substitute inner fromVar toVar
substitute (And e1 e2) fromVar toVar = 
  And (substitute e1 fromVar toVar) (substitute e2 fromVar toVar)
substitute (Or e1 e2) fromVar toVar = 
  Or (substitute e1 fromVar toVar) (substitute e2 fromVar toVar)
substitute (Xor e1 e2) fromVar toVar = 
  Xor (substitute e1 fromVar toVar) (substitute e2 fromVar toVar)



