module Idyllic.Typing.Kind where

data Kind
  = KindStar -- Type
  | KindArrow Kind Kind -- Function type
  deriving (Show, Eq)