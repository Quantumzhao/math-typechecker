module BinaryOperation where
import Set
import Node
import ContextState
-- import Relation

-- data Associativity = Assoc | NotAssoc

-- data BinaryOperation = BinOp Set Set Set Associativity

-- isClosed (BinOp sa sb sc _) = 
--   sa `setEqual` sb && sb `setEqual` sc 

toSubsetL :: Graph -> Node -> Node -> Node
toSubsetL graph lv' binop@(Binary lv rv o ts) = 
  if isSubsetOf' graph lv' lv && lv' /= lv then Binary lv' rv o ts
  else binop
toSubsetL _ _ n = n

toSubsetR :: Graph -> Node -> Node -> Node
toSubsetR graph rv' binop@(Binary lv rv o ts) = 
  if isSubsetOf' graph rv' rv && rv' /= rv then Binary lv rv' o ts
  else binop
toSubsetR _ _ n = n

toRelationR ((), r) binop = undefined

toRelationL (l, ()) binop = undefined
