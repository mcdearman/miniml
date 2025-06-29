module MMC.Layout (layout, LayoutError) where

import Data.Text (Text)
import MMC.Common (Loc (..), Located (..))
import MMC.Token (LToken, Token (..))
import MMC.TokenTree (LTokenTree)

data LayoutError = LayoutError Text deriving (Show, Eq)

layout :: [LToken] -> [Int] -> Either LayoutError [LTokenTree]
layout ts [0] = undefined
layout _ [] = error "layout stack underflow"
