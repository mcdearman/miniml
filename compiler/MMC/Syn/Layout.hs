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

data Cursor = Cursor
  { tokens :: [Token],
    stack :: [Frame],
    offset :: !Int,
    col :: !Int
  }

data Frame = Braced | Layout Int deriving (Show, Eq)

type LayoutM = State Cursor

runLayout :: [Token] -> PipelineM [VTok]
runLayout ts = do
  li <- asks pipelineLineIndex
  pure $ evalState (layout li) (Cursor ts [] 0 1)

layout :: LineIndex -> LayoutM [VTok]
layout li = undefined

bump :: LineIndex -> LayoutM VTok
bump li = do
  Cursor ts stack offset col <- get
  case ts of
    [] -> error "no more tokens"
    (t : ts') -> do
      put $ Cursor ts' stack (offset + tokenLength t) (col + 1)
      pure $ VTok t

-- layout :: LineIndex -> Int -> [Token] -> [Int] -> [VTok]
-- --  L ({n} : ts) [] = {  :  (L ts [n]) if n > 0 (Note 1)
-- layout li offset (t : c : ts) []
--   | isLayoutHerald t c =
--       let (_, col) = offsetToLineCol li offset
--        in VIndent col : layout li col (c : ts) [col]
-- layout li col (t : ts) (0 : ms) | tokenKind t == SyntaxKindRBrace = VTok t : layout li ts ms
-- layout _ _ (t : ts) ms | tokenKind t == SyntaxKindRBrace = error "unmatched right brace"
-- layout li _ (t : ts) ms | tokenKind t == SyntaxKindLBrace = VTok t : layout li ts (0 : ms)
-- layout li _ (t : ts) (m : ms) | m /= 0 = VDedent m : layout li (t : ts) ms
-- layout li _ (t : ts) ms = VTok t : layout li ts ms
-- layout _ _ [] [] = []
-- layout li _ [] (m : ms) | m /= 0 = VDedent m : layout li [] ms

-- layout _ _ _ = undefined
isLayoutHerald :: Token -> Token -> Bool
isLayoutHerald t c = isLayoutKeyword t && tokenKind c == SyntaxKindColon