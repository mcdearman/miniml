module Idyllic.Syn.Layout where

-- import Control.Monad.Reader (MonadReader (ask))
-- import Control.Monad.Reader.Class (asks)
-- import Control.Monad.State (MonadState (get, put), evalStateT)
-- import Control.Monad.State.Lazy
-- import Data.ByteString (ByteString)
-- import Data.Text.Lazy (toStrict, unpack)
-- import Debug.Trace (trace)
-- import MMC.Build.Effect (HasDiagnostic (..), PipelineEnv (..), PipelineM)
-- import MMC.Syn.GreenNode (SyntaxKind (..), Token (..), isLayoutKeyword, tokenLength)
-- import MMC.Syn.TokenTree (Delim)
-- import MMC.Utils.LineIndex (LineIndex, offsetToLineCol)

-- data Event
--   = EventTok !Token
--   | EventStartGroup !Delim
--   | EventCloseGroup !Delim
--   | EventRef !Int
--   | EventSentinel !Int
--   deriving (Show, Eq)

-- data VTok
--   = VTok !Token
--   | VIndent !Int
--   | VDedent !Int
--   | VSemi !Int
--   deriving (Show, Eq)

-- -- let: x = 1
-- --      y = 2
-- --  in x + y

-- -- data Cursor = Cursor
-- --   { tokens :: [Token],
-- --     stack :: [Frame],
-- --     offset :: !Int,
-- --     col :: !Int
-- --   }

-- -- type LayoutM = State Cursor

-- -- runLayout :: [Token] -> PipelineM [VTok]
-- -- runLayout ts = do
-- --   li <- asks pipelineLineIndex
-- --   pure $ evalState (layout li) (Cursor ts [] 0 1)

-- -- layout :: LineIndex -> LayoutM [VTok]
-- -- layout li = undefined

-- -- bump :: LineIndex -> LayoutM VTok
-- -- bump li = do
-- --   Cursor ts stack offset col <- get
-- --   case ts of
-- --     [] -> error "no more tokens"
-- --     (t : ts') -> do
-- --       put $ Cursor ts' stack (offset + tokenLength t) (col + 1)
-- --       pure $ VTok t

-- layout :: LineIndex -> [Token] -> [VTok]
-- layout li ts = go ts 0 [1]
--   where
--     go :: [Token] -> Int -> [Int] -> [VTok]
--     --  L ({n} : ts) [] = {  :  (L ts [n]) if n > 0 (Note 1)
--     go (t : c : ts) o []
--       | isLayoutHerald t c =
--           let (_, col) = offsetToLineCol li o
--            in VIndent col : go (c : ts) col [col]
--     go (t : ts) col (0 : ms) | tokenKind t == SyntaxKindRBrace = VTok t : go ts col ms
--     go (t : ts) _ ms | tokenKind t == SyntaxKindRBrace = error "unmatched right brace"
--     go (t : ts) o ms | tokenKind t == SyntaxKindLBrace = VTok t : go ts o (0 : ms)
--     go (t : ts) o (m : ms) | m /= 0 = VDedent m : go (t : ts) o ms
--     go (t : ts) o ms = VTok t : go ts o ms
--     go [] _ [] = []
--     go [] o (m : ms) | m /= 0 = VDedent m : go [] o ms
--     go _ _ _ = undefined

-- isLayoutHerald :: Token -> Token -> Bool
-- isLayoutHerald t c = isLayoutKeyword t && tokenKind c == SyntaxKindColon