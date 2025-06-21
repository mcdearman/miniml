module MMC.Layout (runLayout, layout) where

import Data.Text (Text)
import MMC.Common (Loc (..), Located (..))
import MMC.Token (LToken, Token (..))

runLayout :: Text -> [LToken] -> [LToken]
runLayout src tokens = undefined

layout :: [LToken] -> [Int] -> [LToken]
layout (Located (TokWhitespace n) l : ts) (m : ms)
  | m == n = Located TokSemi l : layout ts (m : ms)
  | n < m = Located TokRBrace l : layout (Located (TokWhitespace n) l : ts) ms
layout (t@(Located TokRBrace _) : ts) (0 : ms) = t : layout ts ms
layout (Located TokRBrace _ : _) _ = error "Layout error: Unexpected right brace"
layout (t@(Located TokLBrace _) : ts) ms = t : layout ts (0 : ms)
layout (t : ts) ms = t : layout ts ms
layout [] [] = []
layout [] (m : ms) | m /= 0 = Located TokRBrace (Loc 0 0) : layout [] ms
layout [] _ = error "Layout error: Mismatched braces or missing tokens"
