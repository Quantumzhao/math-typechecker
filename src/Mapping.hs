module Mapping where

data Bijectivity = Injective | Surjective | Bijective

data Partiality = None | Partial

data Mapping a b = Map a b Bijectivity Partiality


