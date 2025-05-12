module TMC.Pipeline where

import Control.Monad.State (StateT)
import Control.Monad.State.Strict
import Control.Placeholder (todo)
import Data.Function ((&))
import Data.Map
import qualified Data.Map as Map
import Data.Text (Text, pack, unpack)
import Data.Text.Lazy (toStrict)
import Debug.Trace (trace)
import Infer (Solver (..), defaultSolver, infer)
import qualified Infer as Solver
import Lexer (tokenize)
import Parser (parseStream)
import Rename
import qualified Rename as Resolver
import Text.Megaparsec (errorBundlePretty)
import Text.Pretty.Simple (pShow)

data PipelineEnv = PipelineEnv
  { src :: Text,
    flags :: [Text],
    resolver :: Resolver,
    solver :: Solver
  }
  deriving (Show)

defaultPipelineEnv :: PipelineEnv
defaultPipelineEnv =
  PipelineEnv
    { src = "",
      flags = [],
      resolver = defaultResolver,
      solver = defaultSolver
    }

runPipeline :: PipelineEnv -> Text -> (Text, PipelineEnv)
runPipeline src = do
  Compiler {src = _, flags = f, resolver = r, solver = sl} <- get
  put $ Compiler {src = src, flags = f, resolver = r, solver = sl}
  case lexMML src of
    Left err -> pure $ pack $ "Lexer error: " ++ errorBundlePretty err
    Right ts -> trace (unpack . toStrict $ (pShow ts)) $ case parseStream ts of
      Left err -> pure $ pack $ "Parser error: " ++ errorBundlePretty err
      Right p -> do
        putStrLn $ pShow p
