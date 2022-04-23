module Node where

-- data Node
--   = Element
--   | Set {
--     tags :: [String]
--   }
--   | Group {
--     tags :: [String]
--   }
--   | Map {
--     tags :: [String]
--   }
--   | Relation {
--     Node :: StructuralRelation,
--     tags :: [String]
--   }
--   | BinOp {
--     operation :: BinaryOperationType,
--     tags :: [String]
--   }
--   | Statement {
--     dependencies :: [Node]
--   }

-- data BinaryOperationType
--   = Intersect
--   | Union
--   | Cross
--   | RelCompl

-- data StructuralRelation
--   = Subset

data Node
  = Mapping {
    domain :: Node, 
    range :: Node,
    tags :: [String], 
    key :: Identifier
  }
  | Collection {
    defOf :: ElementTemplate,
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
  | Anonymous
  deriving (Show)

instance Eq Identifier where
  (==) (Unique n i) (Unique n' i') = n == n' && i == i'
  (==) _ _ = False

data ElementTemplate 
  -- create from a defined set of elements
  = Multiple [Node]
  -- for all ... in ..., be in this form
  | FormOf Node
  deriving (Eq, Show)
