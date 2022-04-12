module BinaryOperation where
import Set
import Relation

data Associativity = Assoc | NotAssoc

data BinaryOperation = BinOp Set Set Set Associativity

isClosed (BinOp sa sb sc _) = 
  sa `setEqual` sb && sb `setEqual` sc 

toRelationR ((), r) binop = undefined

toRelationL (l, ()) binop = undefined
