module MMC.Driver (runDriver) where

import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import MMC.Pipeline (PipelineEnv)
import MMC.Syn.Lexer (tokenize)
import MMC.Syn.Token (Token)

runDriver :: PipelineEnv -> Text -> IO [Token]
runDriver env src = do
  let ts = case tokenize src of
        Left err -> error (show err)
        Right tokens -> tokens
  -- pure $ runReaderT () env
  pure ts
