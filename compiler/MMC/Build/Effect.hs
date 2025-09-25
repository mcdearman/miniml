{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MMC.Build.Effect where

import Control.Concurrent.STM (TVar, newTVarIO)
import Control.Monad.Reader (MonadReader (ask), ReaderT)
import Control.Monad.State
import Data.ByteString (ByteString)
import Data.Text (Text)
import Error.Diagnose (Diagnostic)
import MMC.Utils.LineIndex (LineIndex, buildLineIndex)

data InputMode
  = InputModeFile Text
  | InputModeInteractive
  deriving (Show, Eq)

data PipelineEnv = PipelineEnv
  { pipelineSrc :: !ByteString,
    pipelineFlags :: ![Text],
    pipelineMode :: !InputMode,
    pipelineLineIndex :: !LineIndex,
    pipelineErrors :: !(TVar [Diagnostic Text])
  }
  deriving (Eq)

class HasDiagnostic e where
  toDiagnostic :: e -> Diagnostic Text

defaultPipelineEnv :: ByteString -> IO PipelineEnv
defaultPipelineEnv src = do
  errVar <- newTVarIO []
  pure
    PipelineEnv
      { pipelineSrc = src,
        pipelineFlags = [],
        pipelineMode = InputModeInteractive,
        pipelineLineIndex = buildLineIndex src,
        pipelineErrors = errVar
      }

type PipelineM = ReaderT PipelineEnv IO