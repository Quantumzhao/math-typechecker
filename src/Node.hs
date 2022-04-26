{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Node where

data Node
  = ToClass Class
  | ToRelation Relation
  | ToMapping Mapping
  | ToTuple DirectProduct
  | ToObject Object
  deriving (Eq, Show)

data Object = Object {
  key_O :: Identifier
} deriving (Eq, Show)

data Relation = Relation {
  domain_R :: Node,
  codomain_R :: Node,
  tags_R :: [String], 
  key_R :: Identifier
} deriving (Eq, Show)

data Class = Class  {
  -- variables_C :: [String],
  property_C :: BooleanExpression,
  tags_C :: [String], 
  key_C :: Identifier
} deriving (Eq, Show)

data DirectProduct = DirectProduct {
  entries_D :: (Node, Node),
  key_D :: Identifier
} deriving (Eq, Show)

data Mapping = Mapping {
  domain_M :: Node, 
  range_M :: Node,
  tags_M :: [String], 
  key_M :: Identifier
} deriving (Eq, Show)


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
  (==) _ _ = Prelude.False

data BooleanExpression
  = Not BooleanExpression 
  | And BooleanExpression BooleanExpression 
  | Or BooleanExpression BooleanExpression
  | TupleAnd BooleanExpression BooleanExpression
  | False
  | True
  -- it will evaluate the qualifier to either true or false
  -- the node should only be a mapping
  | Characteristic Mapping
  deriving (Eq, Show)
