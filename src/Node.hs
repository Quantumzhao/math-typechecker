module Node where

data Node
  = Mapping {
    domain :: Node, 
    range :: Node,
    tags :: [String], 
    key :: Identifier
  }
  | Class {
    -- defOf :: ElementTemplate,
    tags :: [String], 
    key :: Identifier
  }
  | DirectProduct {
    entries :: (Node, Node),
    key :: Identifier
  }
  -- the most generic type of objects, can be any type
  | Object {
    typeOf :: Node,
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
  deriving (Eq, Show)

data UpperStructure
  = App {
    transformation :: Node,
    fixed :: Node
  }
  | Definition
  deriving (Eq, Show)

data Identifier
  = Unique {
    nameOf :: String,
    id :: String
  }
  | Arbitrary
  deriving (Show)

instance Eq Identifier where
  (==) (Unique n i) (Unique n' i') = n == n' && i == i'
  (==) _ _ = Prelude.False

data ElementTemplate 
  -- create from a defined set of elements
  = Multiple [Node]
  | FormOf Node
  deriving (Eq, Show)
