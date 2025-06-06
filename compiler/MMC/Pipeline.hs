module MMC.Pipeline (PipelineEnv (..), defaultPipelineEnv, Pipeline, runPipeline) where

import Control.Monad.State (MonadState (get, put), State)
import Data.Function ((&))
import Data.Map
import qualified Data.Map as Map
import Data.Text (Text, pack, unpack)
import Data.Text.Lazy (toStrict)
import Debug.Trace (trace)
import MMC.Common (InputMode (InputModeFile))
import MMC.Parser (parseMML)
import Text.Megaparsec (errorBundlePretty)
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

runPipeline :: InputMode -> Text -> Pipeline Text
runPipeline mode src = do
  PipelineEnv {src = _, flags = f} <- get
  put $ PipelineEnv {src = src, flags = f}
  case parseMML mode src of
    Left err -> pure $ pack $ "Parser error: " ++ errorBundlePretty err
    Right p -> do
      pure $ toStrict $ pShow p
