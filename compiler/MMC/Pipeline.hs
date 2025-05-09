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

-- let (nir, r') = runState (rename p) r
-- let r'' = r' & \res -> res {Resolver.errors = []}
-- case r' of
--   Resolver {errors = []} -> do
--     put $ Compiler {src = src, flags = f, resolver = r', solver = sl}
--     -- trace (unpack . toStrict $ pShow nir) $ pure ()
--     let (tir, sl') = runState (infer nir) sl
--     let sl'' = sl' & \s -> s {Solver.errors = []}
--     case sl' of
--       Solver {errors = []} -> do
--         put $ Compiler {src = src, flags = f, resolver = r', solver = sl'}
--         pure $ (toStrict . pShow) tir
--       Solver {errors = es} -> do
--         put $ Compiler {src = src, flags = f, resolver = r', solver = sl''}
--         pure $ pack $ "Type errors: " ++ show es
--   Resolver {errors = es} -> do
--     put $ Compiler {src = src, flags = f, resolver = r'', solver = sl}
--     pure $ pack $ "Resolver errors: " ++ show es
