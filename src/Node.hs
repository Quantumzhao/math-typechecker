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
  -- an alias of an existing object
  | Alias {
    reference :: Node,
    key :: Identifier
  }
  | ClaimOfRel {
    from :: Node,
    to :: Node,
    relation :: Node,
    -- the key is never used, it's just a placeholder to suppress the record selector error
    key :: Identifier
  }
  deriving (Eq, Show)

-- the qualifiers
data Identifier
  = Exist {
    nameOf :: String,
    id :: String
  }
  | ForAll
  deriving (Show)

-- two variables of `forall` qualifiers should never be equal
instance Eq Identifier where
  (==) (Exist n i) (Exist n' i') = n == n' && i == i'
  (==) _ _ = Prelude.False
