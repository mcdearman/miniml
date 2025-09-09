module MMC.Syn.Layout (LayoutError) where

import Control.Monad.Reader (MonadReader (ask))
import Control.Monad.Reader.Class (asks)
import Control.Monad.State (State)
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
  { tokens :: [Token],
    index :: LineIndex,
    stack :: [Int],
    offset :: Int,
    col :: Int
  }

-- runLayout :: [Token] -> PipelineM [VTok]
-- runLayout ts = do
--   PipelineEnv {pipelineSrc = src, pipelineLineIndex = index} <- ask
--   let cursor = LayoutCursor {tokens = ts, index = index, stack = [0], offset = 0, col = 0}
--   undefined

-- let: x = 1
--      y = 2
--  in x + y

type LayoutS = State LayoutCursor

layout :: LayoutCursor -> LayoutS VTok
layout c = layout' (tokens c) [0]
  where
    layout' ts [0] = undefined
    layout' _ [] = error "layout stack underflow"
    layout' _ _ = undefined