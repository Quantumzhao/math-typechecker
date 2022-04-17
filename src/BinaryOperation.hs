module BinaryOperation where
import Set
import Node
import ContextState
-- import Relation

-- data Associativity = Assoc | NotAssoc

-- data BinaryOperation = BinOp Set Set Set Associativity

-- isClosed (BinOp sa sb sc _) = 
--   sa `setEqual` sb && sb `setEqual` sc 

-- toSubsetL :: Graph -> Node -> Node -> Node
-- toSubsetL graph lv' binop@(Binary lv rv o ts) = 
--   if isSubsetOf' graph lv' lv && lv' /= lv then Binary lv' rv o ts
--   else binop
-- toSubsetL _ _ n = n

-- toSubsetR :: Graph -> Node -> Node -> Node
-- toSubsetR graph rv' binop@(Binary lv rv o ts) = do
--   isSubset <- isSubsetOf' rv' rv
--   if isSubset && rv' /= rv then return Binary lv rv' o ts
--   else binop
-- toSubsetR _ _ n = n

toRelationR ((), r) binop = undefined

toRelationL (l, ()) binop = undefined

relCompose :: Record -> Record -> PContext Record  
relCompose (name1, Relation f1 t1 tags1) (name2, Relation f2 t2 tags2) = do
  nodes <- getNodes 
  isSubset <- t1 `isSubsetOf'` f2
  if isSubset then do
    let newRelation = Relation f1 t2 []
    return $ record (name1 ++ "compose" ++ name2) newRelation
  else error "relationalCompose: mismatch"
relCompose _ _ = error "relationalCompose: not relations"

  

