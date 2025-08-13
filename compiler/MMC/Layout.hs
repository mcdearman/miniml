module MMC.Layout (layout, LayoutError) where

import Data.Text (Text)
import MMC.Common (Loc (..), Located (..))
import MMC.Token (LToken, Token (..))
import MMC.TokenTree (LTokenTree)

data LayoutError = LayoutError Text deriving (Show, Eq)

insertIndents :: [LToken] -> [LToken]
insertIndents = go 0
  where
    go _ [] = []
    go n (t:ts) = case t of


layout :: [LToken] -> [Int] -> Either LayoutError [LTokenTree]
layout ts stack = layout' ts stack [0]
  where
    layout' ts [0] = undefined
    layout' _ [] = error "layout stack underflow"
    layout' _ _ = undefined
