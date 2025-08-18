module MMC.Layout (layout, LayoutError) where

import Data.Text (Text)
import MMC.Common (Loc (..), Located (..), unLoc)
import MMC.Token
import MMC.TokenTree (LTokenTree)

data LayoutError = LayoutError Text deriving (Show, Eq)

insertIndents :: [LToken] -> [LRawTok]
insertIndents [] = []
insertIndents (herald : ref : c : ts) = case unLoc herald of
  TokLet; TokDo; TokWhere; TokMatch -> case ref of
    Located TokLBrace l -> insertIndents (ref : ts)
    Located t l -> (Located (RawTokRef n) l) : insertIndents (ref : ts)
  _ -> insertIndents (ref : ts)

-- insertIndents (t : t' : ts) = (Located (RawTokRef n) l) : insertIndents ts

layout :: [LRawTok] -> [Int] -> Either LayoutError [LToken]
layout ts stack = layout' ts stack [0]
  where
    layout' ts [0] = undefined
    layout' _ [] = error "layout stack underflow"
    layout' _ _ = undefined