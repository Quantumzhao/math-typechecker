module Node where

data Node
  = Mapping {
    domain :: Node, 
    range :: Node,
    tags :: [String], 
    key :: Identifier
  }
  | Class {
    tags :: [String], 
    key :: Identifier
  }
  | DirectProduct {
    entries :: (Node, Node),
    key :: Identifier
  }
  -- the most generic type of objects, can be any type
  | Object {
    key :: Identifier
  } 
  | Relation {
    domain :: Node,
    codomain :: Node,
    tags :: [String], 
    key :: Identifier
  }
  | Alias {
    reference :: Node,
    key :: Identifier
  }
  | ClaimOfRel {
    from :: Node,
    to :: Node,
    relation :: Node,
    key :: Identifier
  }
  deriving (Eq, Show)

data Identifier
  = Exist {
    nameOf :: String,
    id :: String
  }
  | ForAll
  deriving (Show)

instance Eq Identifier where
  (==) (Exist n i) (Exist n' i') = n == n' && i == i'
  (==) _ _ = Prelude.False
