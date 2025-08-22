{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MMC.Pipeline (runPipeline) where

import Control.Monad.Reader (ReaderT (runReaderT))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy (toStrict)
import MMC.AST (Prog)
import MMC.Build (PipelineEnv, PipelineM)
import MMC.Common
import MMC.Lexer (tokenize)
import MMC.Parser (parseMML)
import MMC.Token (LToken, Token)
import Text.Megaparsec (errorBundlePretty)
import Text.Megaparsec.Error (ParseErrorBundle)
import Text.Pretty.Simple (pShow)

runPipeline :: PipelineEnv -> Text -> IO [LToken]
runPipeline env src = do
  let ts = tokenize $ BL.fromStrict $ encodeUtf8 src
  -- pure $ runReaderT () env
  pure ts

-- putStrLn . unpack . toStrict $ pShow out