module Idyllic.Build.Driver (runDriver) where

import Control.Monad.Reader (ReaderT (runReaderT))
import Data.ByteString (ByteString)
import Idyllic.Build.Effect (PipelineEnv (..))
import Idyllic.Build.Pipeline (runPipeline)

runDriver :: PipelineEnv -> ByteString -> IO ()
runDriver env src = do
  runReaderT (runPipeline (pipelineMode env) src) env
