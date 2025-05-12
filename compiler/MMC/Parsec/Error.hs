module MMC.Parsec.Error where

import Data.Text (Text)

data ParseError = ParseError
  { error :: Int,
    errorMsg :: Text
  }
  deriving (Eq, Show)