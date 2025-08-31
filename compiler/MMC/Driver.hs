module MMC.Driver (runDriver) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import MMC.Pipeline (PipelineEnv)
import MMC.Syn.Lexer (tokenize)
import MMC.Syn.Token (Token)

runDriver :: PipelineEnv -> ByteString -> IO [Token]
runDriver env src = do
  let ts = tokenize (BL.fromStrict src)
  pure ts
