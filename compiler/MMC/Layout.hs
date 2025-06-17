module Layout where

import Data.Text (Text)
import MMC.Common (Loc (..), Located (..))
import MMC.Token (LToken, Token (TokLBrace))

runLayout :: Text -> [LToken] -> [LToken]
runLayout src tokens = undefined

layout :: [LToken] -> [Int] -> [LToken]

layout (t : ts) ms = t : layout ts ms
layout [] [] = []
layout [] (m : ms) = Located TokLBrace (Loc 0 0) : layout [] ms
