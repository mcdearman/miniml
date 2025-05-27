module MMC.Pipeline (PipelineEnv (..), defaultPipelineEnv, Pipeline, runPipeline) where

import Control.Monad.State (MonadState (get, put), State)
import Data.Function ((&))
import Data.Map
import qualified Data.Map as Map
import Data.Text (Text, pack, unpack)
import Data.Text.Lazy (toStrict)
import Debug.Trace (trace)
-- import Infer (Solver (..), defaultSolver, infer)
-- import qualified Infer as Solver
import MMC.Lexer (tokenize)
-- import Parser (parseStream)
-- import Rename
-- import qualified Rename as Resolver
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

runPipeline :: Text -> Pipeline Text
runPipeline src = do
  PipelineEnv {src = _, flags = f} <- get
  put $ PipelineEnv {src = src, flags = f}
  case tokenize src of
    Left err -> pure $ pack $ "Lexer error: " ++ errorBundlePretty err
    Right ts -> pure $ toStrict $ pShow ts

-- Right ts -> trace (unpack . toStrict $ (pShow ts)) $ case parseStream ts of
--   Left err -> pure $ pack $ "Parser error: " ++ errorBundlePretty err
--   Right p -> do
--     putStrLn $ pShow p
