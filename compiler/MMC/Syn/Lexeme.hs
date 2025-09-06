module MMC.Syn.Lexeme where

import MMC.Syn.SyntaxKind (SyntaxKind)

data Lexeme = Lexeme
  { lexemeKind :: SyntaxKind,
    lexemeText :: String,
    lexemeCol :: Int
  }
  deriving (Show, Eq, Ord)