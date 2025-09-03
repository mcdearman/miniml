module MMC.Syn.GreenNode where

import MMC.Syn.SyntaxKind (SyntaxKind)
import MMC.Syn.Token (Token)
import MMC.Utils.Unique (Unique)

data GreenNode = GreenNode
  { greenNodeId :: Unique,
    greenNodeKind :: SyntaxKind,
    greenNodeChildren :: [Either Token GreenNode],
    greenNodeWidth :: Int
  }