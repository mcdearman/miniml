module MMC.Syn.SyntaxNode where

import MMC.Syn.GreenNode (GreenNode)
import MMC.Utils.Unique (Unique)

data SyntaxNode = SyntaxNode
  { syntaxNodeId :: Unique,
    syntaxNodeOffset :: Int,
    syntaxNodeParent :: Maybe SyntaxNode,
    syntaxNodeGreen :: GreenNode
  }