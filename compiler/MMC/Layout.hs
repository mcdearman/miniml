module MMC.Layout (layout, LayoutError) where

import Data.Text (Text)
import qualified Data.Text as T
import MMC.Common (Loc (..), Located (..), unLoc)
import MMC.Token
import MMC.TokenTree (LTokenTree)

data LayoutError = LayoutError Text deriving (Show, Eq)

-- let: x = 1
--      y = 2
--  in x + y

insertIndents :: [LToken] -> [LRawTok]
insertIndents [] = []
insertIndents (herald : c : ref : ts) = case unLoc herald of
  TokLet; TokDo; TokWhere; TokMatch -> case c of
    Located TokLBrace _ -> insertIndents (ref : ts)
    Located TokColon l -> undefined
    _ -> undefined
  -- (Located (RawTokRef n) l) : insertIndents (ref : ts)
  _ -> insertIndents (ref : ts)
insertIndents _ = error "insertIndents: unexpected token structure"

-- insertIndents (t : t' : ts) = (Located (RawTokRef n) l) : insertIndents ts

layout :: [LRawTok] -> [Int] -> Either LayoutError [LToken]
layout ts stack = layout' ts stack [0]
  where
    layout' ts [0] = undefined
    layout' _ [] = error "layout stack underflow"
    layout' _ _ = undefined