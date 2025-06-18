module Layout where

import Data.Text (Text)
import MMC.Common (Loc (..), Located (..))
import MMC.Token (LToken, Token (..))

runLayout :: Text -> [LToken] -> [LToken]
runLayout src tokens = undefined

layout :: [LToken] -> [Int] -> [LToken]
layout (Located (TokWhitespace n) l : ts) (m : ms)
  | m == n = Located TokSemi l : layout ts (m : ms)
  | n < m = Located TokLBrace l : layout (Located (TokWhitespace n) l : ts) ms
layout (t : ts) ms = t : layout ts ms
layout [] [] = []
layout [] (m : ms) | m /= 0 = Located TokLBrace (Loc 0 0) : layout [] ms
