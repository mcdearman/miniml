{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MMC.Pipeline (PipelineEnv (..), defaultPipelineEnv, runPipeline) where

import Control.Monad.Reader (MonadReader (ask), ReaderT)
import Control.Monad.State (MonadState (get, put), State)
import Data.Bifunctor (Bifunctor (first))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Function ((&))
import Data.Map
import qualified Data.Map as Map
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy (toStrict)
import Data.Void (Void)
import Debug.Trace (trace)
import Error.Diagnose
import Error.Diagnose.Compat.Megaparsec (HasHints (..), errorDiagnosticFromBundle)
import MMC.AST (Prog)
import MMC.Common (InputMode (InputModeFile), Loc)
import MMC.Lexer (tokenize)
import MMC.Parser (parseMML)
import MMC.Token (LToken, Token)
import Text.Megaparsec (errorBundlePretty)
import Text.Megaparsec.Error (ParseErrorBundle)
import Text.Pretty.Simple (pShow)

data PipelineEnv = PipelineEnv
  { src :: Text,
    flags :: [Text],
    errors :: [CompilerError]
  }
  deriving (Show, Eq)

data CompilerError
  = LexerError Loc
  | ParserError (ParseErrorBundle Text Void)
  deriving (Show, Eq)

defaultPipelineEnv :: PipelineEnv
defaultPipelineEnv =
  PipelineEnv
    { src = "",
      flags = [],
      errors = []
    }

type PipelineM = ReaderT PipelineEnv IO

runPipeline :: InputMode -> Text -> PipelineM [LToken]
runPipeline mode src = do
  PipelineEnv {src = _, flags = f} <- ask
  pure $ tokenize $ BL.fromStrict $ encodeUtf8 src