module MMC.Syn.GreenNode
  ( GreenNode (..),
    Token (..),
    SyntaxKind (..),
    tokenLength,
    isTrivia,
    isSpace,
    isKeyword,
    isLayoutKeyword,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import MMC.Utils.Unique (Unique)

data GreenNode = GreenNode
  { greenNodeId :: !Unique,
    greenNodeKind :: !SyntaxKind,
    greenNodeChildren :: [Either Token GreenNode],
    greenNodeWidth :: !Int
  }
  deriving (Show, Eq, Ord)

data Token = Token
  { tokenKind :: !SyntaxKind,
    tokenText :: !ByteString
  }
  deriving (Show, Eq, Ord)

data SyntaxKind
  = SyntaxKindError
  | SyntaxKindTab
  | SyntaxKindWhitespace
  | SyntaxKindNewline
  | SyntaxKindComment
  | SyntaxKindUppercaseIdent
  | SyntaxKindLowercaseIdent
  | SyntaxKindOpIdent
  | SyntaxKindConOpIdent
  | SyntaxKindInt
  | SyntaxKindString
  | SyntaxKindChar
  | SyntaxKindLParen
  | SyntaxKindRParen
  | SyntaxKindLBrace
  | SyntaxKindRBrace
  | SyntaxKindLBracket
  | SyntaxKindRBracket
  | SyntaxKindBang
  | SyntaxKindHash
  | SyntaxKindBackSlash
  | SyntaxKindColon
  | SyntaxKindSemi
  | SyntaxKindComma
  | SyntaxKindPeriod
  | SyntaxKindEq
  | SyntaxKindLArrow
  | SyntaxKindRArrow
  | SyntaxKindLFatArrow
  | SyntaxKindBar
  | SyntaxKindUnderscore
  deriving (Show, Eq, Ord)

tokenLength :: Token -> Int
tokenLength = ByteString.length . tokenText

isTrivia :: Token -> Bool
isTrivia = go . tokenKind
  where
    go SyntaxKindWhitespace = True
    go SyntaxKindNewline = True
    go SyntaxKindTab = True
    go SyntaxKindComment = True
    go _ = False

isSpace :: Token -> Bool
isSpace = go . tokenKind
  where
    go SyntaxKindWhitespace = True
    go _ = False

isKeyword :: Token -> Bool
isKeyword t
  | tokenKind t == SyntaxKindLowercaseIdent = go $ tokenText t
  | otherwise = False
  where
    go "module" = True
    go "import" = True
    go "let" = True
    go "in" = True
    go "where" = True
    go "if" = True
    go "then" = True
    go "else" = True
    go "match" = True
    go "with" = True
    go "record" = True
    go "data" = True
    go "type" = True
    go "class" = True
    go "instance" = True
    go "do" = True
    go _ = False

isLayoutKeyword :: Token -> Bool
isLayoutKeyword t
  | tokenKind t == SyntaxKindLowercaseIdent = go $ tokenText t
  | otherwise = False
  where
    go "let" = True
    go "where" = True
    go "do" = True
    go "with" = True
    go _ = False