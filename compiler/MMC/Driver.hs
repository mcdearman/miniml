module MMC.Driver (runDriver) where

import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import MMC.Pipeline (PipelineEnv)
import MMC.Syn.Lexer (tokenize)
import MMC.Syn.Token (Token)

runDriver :: PipelineEnv -> Text -> IO [Token]
runDriver env src = do
  let (es, ts) = tokenize src
  -- pure $ runReaderT () env
  pure ts
