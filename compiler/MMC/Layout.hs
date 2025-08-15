module MMC.Layout (layout, LayoutError) where

import Data.Text (Text)
import MMC.Common (Loc (..), Located (..))
import MMC.Token
import MMC.TokenTree (LTokenTree)

data LayoutError = LayoutError Text deriving (Show, Eq)

insertIndents :: [LToken] -> [LRawTok]
insertIndents = go 0
  where
    go _ [] = []
    go n (t : ts) = case t of {}

layout :: [LRawTok] -> [Int] -> Either LayoutError [LToken]
layout ts stack = layout' ts stack [0]
  where
    layout' ts [0] = undefined
    layout' _ [] = error "layout stack underflow"
    layout' _ _ = undefined
