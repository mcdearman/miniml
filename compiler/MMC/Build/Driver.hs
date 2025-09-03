module MMC.Build.Driver (runDriver) where

import Control.Monad.Reader (ReaderT (runReaderT))
import Data.ByteString (ByteString)
import MMC.Build.Effect (PipelineEnv (..))
import MMC.Build.Pipeline (runPipeline)

runDriver :: PipelineEnv -> ByteString -> IO ()
runDriver env src = do
  runReaderT (runPipeline (pipelineMode env) src) env
