module MMC.Syn.GreenNode where

import MMC.Syn.SyntaxKind (SyntaxKind)
import MMC.Syn.Token (LToken)

data GreenNode = GreenNode
  { kind :: SyntaxKind,
    children :: [Either LToken GreenNode],
    width :: Int
  }