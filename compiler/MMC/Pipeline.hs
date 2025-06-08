{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MMC.Pipeline (PipelineEnv (..), defaultPipelineEnv, Pipeline, runPipeline) where

import Control.Monad.State (MonadState (get, put), State)
import Data.Function ((&))
import Data.Map
import qualified Data.Map as Map
import Data.Text (Text, pack, unpack)
import Data.Text.Lazy (toStrict)
import Data.Void (Void)
import Debug.Trace (trace)
import Error.Diagnose
import Error.Diagnose.Compat.Megaparsec (HasHints (..), errorDiagnosticFromBundle)
import MMC.AST (Prog)
import MMC.Common (InputMode (InputModeFile))
import MMC.Parser (parseMML)
import Text.Megaparsec (errorBundlePretty)
import Text.Megaparsec.Error (ParseErrorBundle)
import Text.Pretty.Simple (pShow)

data PipelineEnv = PipelineEnv
  { src :: Text,
    flags :: [Text]
  }
  deriving (Show)

defaultPipelineEnv :: PipelineEnv
defaultPipelineEnv =
  PipelineEnv
    { src = "",
      flags = []
    }

type Pipeline = State PipelineEnv

runPipeline :: InputMode -> Text -> Pipeline (Either (ParseErrorBundle Text Void) Prog)
runPipeline mode src = do
  PipelineEnv {src = _, flags = f} <- get
  put $ PipelineEnv {src = src, flags = f}
  pure $ parseMML mode src

-- case parseMML mode src of
-- Left e -> do
--   let diag = errorDiagnosticFromBundle Nothing ("Parse error on input" :: Text) Nothing e
--    in printDiagnostic $ prettyDiagnostic True 2 diag
--   pure $ error ""
-- Right p -> do
--   pure $ p

-- instance HasHints Void msg where
--   hints _ = mempty