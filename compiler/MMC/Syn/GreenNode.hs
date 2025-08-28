module MMC.Syn.GreenNode where

import MMC.Syn.SyntaxKind (SyntaxKind)
import MMC.Syn.Token (Token)

data GreenNode = GreenNode
  { kind :: SyntaxKind,
    children :: [Either Token GreenNode],
    width :: Int
  }