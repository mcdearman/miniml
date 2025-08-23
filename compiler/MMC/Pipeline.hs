{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MMC.Pipeline (HasDiagnostic (..), PipelineEnv (..), defaultPipelineEnv, PipelineM (..)) where

import Control.Concurrent.STM (TVar, newTVarIO)
import Control.Monad.Reader (MonadReader (ask), ReaderT)
import Control.Monad.State
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Error.Diagnose (Diagnostic)
import MMC.Common (InputMode (..), LineIndex (..), buildLineIndex)
import MMC.Syn.Lexer (tokenize)
import MMC.Syn.Token (LToken)

-- putStrLn . unpack . toStrict $ pShow out
data PipelineEnv = PipelineEnv
  { src :: !Text,
    flags :: ![Text],
    mode :: !InputMode,
    lineIndex :: !LineIndex,
    errors :: TVar [Diagnostic Text]
  }
  deriving (Eq)

class HasDiagnostic e where
  toDiagnostic :: e -> Diagnostic Text

defaultPipelineEnv :: Text -> IO PipelineEnv
defaultPipelineEnv src = do
  errVar <- newTVarIO []
  pure
    PipelineEnv
      { src = src,
        flags = [],
        mode = InputModeInteractive,
        lineIndex = buildLineIndex src,
        errors = errVar
      }

-- type PipelineM = ReaderT PipelineEnv IO

newtype PipelineM a = PipelineM {runPipelineM :: ReaderT PipelineEnv IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader PipelineEnv)