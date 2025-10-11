module Idyllic.Build.Pipeline (runPipeline) where

import Control.Concurrent.STM (TVar, newTVarIO)
import Control.Monad.Reader (MonadReader (ask), ReaderT)
import Control.Monad.State
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
-- import Idyllic.Syn.Layout (runLayout)

import Debug.Trace (trace)
import Error.Diagnose (Diagnostic)
import Idyllic.Build.Effect (InputMode, PipelineM)
import Idyllic.Syn.Lexer (tokenize)
import Idyllic.Utils.LineIndex (LineIndex, buildLineIndex)
import Text.Pretty.Simple (pPrint, pShow)

runPipeline :: InputMode -> ByteString -> PipelineM ()
runPipeline mode src = do
  let ts = tokenize (BL.fromStrict src)
  trace "tokens" $ pPrint ts
  -- lts <- runLayout (filter (not . tokenIsSpace) ts)
  pure ()