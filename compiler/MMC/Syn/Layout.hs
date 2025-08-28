module MMC.Syn.Layout (LayoutError) where

import Control.Monad.Reader (MonadReader (ask))
import Control.Monad.Reader.Class (asks)
import Data.Text (Text)
import qualified Data.Text as T
import Error.Diagnose (Diagnostic (..))
import MMC.Common (Loc (..), Located (..), unLoc)
import MMC.Common.LineIndex (LineIndex)
import MMC.Pipeline (HasDiagnostic (..), PipelineEnv (..), PipelineM)
import MMC.Syn.Token
import MMC.Syn.TokenTree (Delim, LTokenTree)

data LayoutError = LayoutError Text deriving (Show, Eq)

instance HasDiagnostic LayoutError where
  toDiagnostic (LayoutError msg) = undefined

data Event
  = EventTok !Token
  | EventStartGroup !Delim
  | EventCloseGroup !Delim
  | EventRef !Int
  | EventSentinel !Int

-- let: x = 1
--      y = 2
--  in x + y

-- runLayout :: [LToken] -> PipelineM [LToken]
-- runLayout tokens = do
--   index <- asks lineIndex
--   let indented = insertIndents tokens index
--       (errs, layoutTokens) = layout indented index [0]
--   pure layoutTokens

-- layout :: [LRawTok] -> LineIndex -> [Int] -> ([LayoutError], [LToken])
-- layout ts index stack = layout' ts stack [0]
--   where
--     layout' ts [0] = undefined
--     layout' _ [] = error "layout stack underflow"
--     layout' _ _ = undefined

-- insertIndents :: [LToken] -> LineIndex -> [LRawTok]
-- insertIndents [] index = []
-- insertIndents (herald : c : ref : ts) index = case unLoc herald of
--   TokLet; TokDo; TokWhere; TokMatch -> case c of
--     Located TokLBrace _ -> insertIndents (ref : ts) index
--     Located TokColon l -> undefined
--     _ -> undefined
--   -- (Located (RawTokRef n) l) : insertIndents (ref : ts)
--   _ -> insertIndents (ref : ts) index
-- insertIndents _ _ = error "insertIndents: unexpected token structure"

-- insertIndents (t : t' : ts) = (Located (RawTokRef n) l) : insertIndents ts
