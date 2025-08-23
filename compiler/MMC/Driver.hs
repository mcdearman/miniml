module MMC.Driver (runDriver) where

import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import MMC.Pipeline (PipelineEnv)
import MMC.Syn.Lexer (tokenize)
import MMC.Syn.Token (LToken)

runDriver :: PipelineEnv -> Text -> IO [LToken]
runDriver env src = do
  let ts = tokenize $ BL.fromStrict $ encodeUtf8 src
  -- pure $ runReaderT () env
  pure ts
