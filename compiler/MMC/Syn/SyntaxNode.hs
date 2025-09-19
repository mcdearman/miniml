module MMC.Syn.SyntaxNode (SyntaxNode (..), nodeKind, nodeChildren) where

import MMC.Syn.GreenNode (GreenNode (..), SyntaxKind)
import MMC.Utils.Unique (Unique)

data SyntaxNode = SyntaxNode
  { syntaxNodeOffset :: !Int,
    syntaxNodeParent :: Maybe SyntaxNode,
    syntaxNodeGreen :: !GreenNode
  }
  deriving (Show, Eq, Ord)

nodeKind :: SyntaxNode -> SyntaxKind
nodeKind node = greenNodeKind (syntaxNodeGreen node)

nodeChildren :: SyntaxNode -> [SyntaxNode]
nodeChildren node = undefined
