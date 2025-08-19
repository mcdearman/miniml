{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MMC.Pipeline (PipelineEnv (..), defaultPipelineEnv, runPipeline, PipelineM, HasDiagnostic (..)) where

import Control.Concurrent.STM (TVar, newTVarIO)
import Control.Monad.Reader (MonadReader (ask), ReaderT)
import Control.Monad.State (MonadState (get, put), State)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy (toStrict)
import Data.Void (Void)
import Debug.Trace (trace)
import Error.Diagnose
import Error.Diagnose.Compat.Megaparsec (HasHints (..), errorDiagnosticFromBundle)
import MMC.AST (Prog)
import MMC.Common (InputMode (InputModeFile, InputModeInteractive), LineIndex, Loc, buildLineIndex)
import MMC.Lexer (tokenize)
import MMC.Parser (parseMML)
import MMC.Token (LToken, Token)
import Text.Megaparsec (errorBundlePretty)
import Text.Megaparsec.Error (ParseErrorBundle)
import Text.Pretty.Simple (pShow)

data PipelineEnv = PipelineEnv
  { src :: !Text,
    flags :: ![Text],
    errors :: TVar [Diagnostic Void],
    mode :: !InputMode,
    lineIndex :: !LineIndex
  }
  deriving (Eq)

class HasDiagnostic e where
  toDiagnostic :: e -> Diagnostic Void

defaultPipelineEnv :: Text -> IO PipelineEnv
defaultPipelineEnv src = do
  errVar <- newTVarIO []
  pure
    PipelineEnv
      { src = src,
        flags = [],
        errors = errVar,
        mode = InputModeInteractive,
        lineIndex = buildLineIndex src
      }

type PipelineM = ReaderT PipelineEnv IO

runPipeline :: Text -> PipelineM [LToken]
runPipeline src = do
  -- PipelineEnv {src = _, flags = f} <- ask
  pure $ tokenize $ BL.fromStrict $ encodeUtf8 src