module Interpreter.Tokens where

data TokenBase
--------- Set -------------------------
  = Set
  | Finite
  | Intersect
  | Union
  | Complement
  | Empty
  | Universal
  | Cross
  | Cardinal
  | PowerSet
  | Sub -- subset
--------- Relation --------------------
  | Bijective
  | Injective
  | Surjective
  | Symmetric
  | Antisymmectric
  | Reflexive
  | Transitive
--------- Boolean Operation -----------
  | And
  | Or
  | Not
  | Xor
--------- Common Sets -----------------
  | Natural
  | Integer
  | Rational
  | Real
  | Complex
--------- Keywords --------------------
  | Where
  | In
  | Of
  | Is
--------- Whatever --------------------
  | Group
--------- Special Characters ----------
  | DefineToBe
  | MapsTo
  | LParen
  | RParen
  | LCurly
  | RCurly
  | ForAll
  | Exist
  | LBrkt
  | RBrkt
  | Addition
  | Multiplication
  | Composition
  | Comma
  | Exponent
