module MMC.Driver (runDriver) where

import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import MMC.Lexer (tokenize)
import MMC.Pipeline (PipelineEnv)
import MMC.Token (LToken)

runDriver :: PipelineEnv -> Text -> IO [LToken]
runDriver env src = do
  let ts = tokenize $ BL.fromStrict $ encodeUtf8 src
  -- pure $ runReaderT () env
  pure ts
