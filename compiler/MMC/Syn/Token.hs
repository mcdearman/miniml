module MMC.Syn.Token
  ( Token (..),
    TokenKind (..),
    tokenIsSpace,
    tokenIsTrivia,
    tokenIsKeyword,
    tokenIsLayoutKeyword,
    tokenText,
  )
where

import Data.ByteString (ByteString)
import MMC.Utils.Span (Span)
import qualified MMC.Utils.Span as S

data Token = Token
  { tokenKind :: !TokenKind,
    tokenSpan :: !Span
  }
  deriving (Show, Eq, Ord)

data TokenKind
  = TokenKindError
  | TokenKindTab
  | TokenKindWhitespace
  | TokenKindNewline
  | TokenKindComment
  | TokenKindUppercaseIdent
  | TokenKindLowercaseIdent
  | TokenKindOpIdent
  | TokenKindConOpIdent
  | TokenKindInt
  | TokenKindString
  | TokenKindChar
  | TokenKindLParen
  | TokenKindRParen
  | TokenKindLBrace
  | TokenKindRBrace
  | TokenKindLBracket
  | TokenKindRBracket
  | TokenKindBang
  | TokenKindHash
  | TokenKindBackSlash
  | TokenKindColon
  | TokenKindSemi
  | TokenKindComma
  | TokenKindPeriod
  | TokenKindEq
  | TokenKindLArrow
  | TokenKindRArrow
  | TokenKindLFatArrow
  | TokenKindBar
  | TokenKindUnderscore
  deriving
    ( -- | TokenKindModule
      -- | TokenKindImport
      -- | TokenKindAs
      -- | TokenKindLet
      -- | TokenKindIn
      -- | TokenKindWhere
      -- | TokenKindIf
      -- | TokenKindThen
      -- | TokenKindElse
      -- | TokenKindMatch
      -- | TokenKindWith
      -- | TokenKindRecord
      -- | TokenKindData
      -- | TokenKindType
      -- | TokenKindClass
      -- | TokenKindInstance
      -- | TokenKindDo
      Show,
      Eq,
      Ord
    )

tokenIsTrivia :: Token -> Bool
tokenIsTrivia = go . tokenKind
  where
    go TokenKindWhitespace = True
    go TokenKindNewline = True
    go TokenKindTab = True
    go TokenKindComment = True
    go _ = False

tokenIsSpace :: Token -> Bool
tokenIsSpace = go . tokenKind
  where
    go TokenKindWhitespace = True
    go _ = False

tokenIsKeyword :: ByteString -> Token -> Bool
tokenIsKeyword src t
  | tokenKind t == TokenKindLowercaseIdent = go $ tokenText src t
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

tokenIsLayoutKeyword :: ByteString -> Token -> Bool
tokenIsLayoutKeyword src t
  | tokenKind t == TokenKindLowercaseIdent = go $ tokenText src t
  | otherwise = False
  where
    go "let" = True
    go "where" = True
    go "do" = True
    go "with" = True
    go _ = False

tokenText :: ByteString -> Token -> ByteString
tokenText src t = S.slice (tokenSpan t) src
