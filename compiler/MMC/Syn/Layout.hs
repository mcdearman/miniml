module MMC.Syn.Layout where

import Control.Monad.Reader (MonadReader (ask))
import Control.Monad.Reader.Class (asks)
import Control.Monad.State (MonadState (get, put), evalStateT)
import Control.Monad.State.Lazy
import Data.ByteString (ByteString)
import Data.Text.Lazy (toStrict, unpack)
import Debug.Trace (trace)
import MMC.Build.Effect (HasDiagnostic (..), PipelineEnv (..), PipelineM)
import MMC.Syn.GreenNode (SyntaxKind (..), Token (..), isLayoutKeyword, tokenLength)
import MMC.Syn.TokenTree (Delim)
import MMC.Utils.LineIndex (LineIndex, offsetToLineCol)

data Event
  = EventTok !Token
  | EventStartGroup !Delim
  | EventCloseGroup !Delim
  | EventRef !Int
  | EventSentinel !Int
  deriving (Show, Eq)

data VTok
  = VTok !Token
  | VIndent !Int
  | VDedent !Int
  | VSemi !Int
  deriving (Show, Eq)

-- let: x = 1
--      y = 2
--  in x + y

-- data Cursor = Cursor
--   { tokens :: [Token],
--     stack :: [Frame],
--     offset :: !Int,
--     col :: !Int
--   }

-- type LayoutM = State Cursor

-- runLayout :: [Token] -> PipelineM [VTok]
-- runLayout ts = do
--   li <- asks pipelineLineIndex
--   pure $ evalState (layout li) (Cursor ts [] 0 1)

-- layout :: LineIndex -> LayoutM [VTok]
-- layout li = undefined

-- bump :: LineIndex -> LayoutM VTok
-- bump li = do
--   Cursor ts stack offset col <- get
--   case ts of
--     [] -> error "no more tokens"
--     (t : ts') -> do
--       put $ Cursor ts' stack (offset + tokenLength t) (col + 1)
--       pure $ VTok t

layout :: LineIndex -> [Token] -> [VTok]
layout li ts = go li 0 ts []
  where
    go :: LineIndex -> Int -> [Token] -> [Int] -> [VTok]
    --  L ({n} : ts) [] = {  :  (L ts [n]) if n > 0 (Note 1)
    go li offset (t : c : ts) []
      | isLayoutHerald t c =
          let (_, col) = offsetToLineCol li offset
           in VIndent col : go li col (c : ts) [col]
    go li col (t : ts) (0 : ms) | tokenKind t == SyntaxKindRBrace = VTok t : go li col ts ms
    go _ _ (t : ts) ms | tokenKind t == SyntaxKindRBrace = error "unmatched right brace"
    go li o (t : ts) ms | tokenKind t == SyntaxKindLBrace = VTok t : go li o ts (0 : ms)
    go li o (t : ts) (m : ms) | m /= 0 = VDedent m : go li o (t : ts) ms
    go li o (t : ts) ms = VTok t : go li o ts ms
    go _ _ [] [] = []
    go li o [] (m : ms) | m /= 0 = VDedent m : go li o [] ms
    go _ _ _ _ = undefined

isLayoutHerald :: Token -> Token -> Bool
isLayoutHerald t c = isLayoutKeyword t && tokenKind c == SyntaxKindColon