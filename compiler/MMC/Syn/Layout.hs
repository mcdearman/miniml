module MMC.Syn.Layout (LayoutError) where

import Control.Monad.Reader (MonadReader (ask))
import Control.Monad.Reader.Class (asks)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict, unpack)
import Debug.Trace (trace)
import Error.Diagnose (Diagnostic (..))
import MMC.Build.Effect (HasDiagnostic (..), PipelineEnv (..), PipelineM)
import MMC.Syn.GreenNode (Token)
import MMC.Syn.TokenTree (Delim)
import MMC.Utils.LineIndex (LineIndex, offsetToLineCol)
import MMC.Utils.Span (Span (..))
import Text.Pretty.Simple (pShow)

data LayoutError = LayoutError Text deriving (Show, Eq)

instance HasDiagnostic LayoutError where
  toDiagnostic (LayoutError msg) = undefined

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

data LayoutCursor = LayoutCursor
  { layoutTokens :: [Token],
    layoutIndex :: LineIndex
  }

runLayout :: [Token] -> PipelineM [Token]
runLayout ts = do
  PipelineEnv {pipelineSrc = src, pipelineLineIndex = index} <- ask
  let es = generateEvents src index ts
  trace (unpack $ pShow es) undefined

generateEvents :: ByteString -> LineIndex -> [Token] -> [Event]
generateEvents src li = undefined

-- where
--   go [] = []
--   go ts'@(kw : c : r : ts) | tokenIsLayoutKeyword kw =
--     case tokenKind c of
--       TokenKindLBrace -> EventTok kw : go ts'
--       TokenKindColon ->
--         let (_, col) = offsetToLineCol li $ spanStart $ tokenSpan r
--          in EventTok kw : EventTok c : EventSentinel col : go (r : ts)
--       _ -> EventTok kw : go (c : r : ts)
--   go (t : ts') = EventTok t : go ts'

-- let: x = 1
--      y = 2
--  in x + y

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
