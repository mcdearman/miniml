module MMC.Syn.Token (Token (..), TokenKind (..)) where

import MMC.Common (Loc)

data Token = Token
  { tokenKind :: !TokenKind,
    tokenLoc :: !Loc
  }
  deriving (Show, Eq, Ord)

data TokenKind
  = TokenKindError
  | TokenKindWhitespace
  | TokenKindNewline
  | TokenKindComment
  | TokenKindUpperCaseIdent
  | TokenKindLowerCaseIdent
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
  | TokenKindModule
  | TokenKindImport
  | TokenKindAs
  | TokenKindLet
  | TokenKindIn
  | TokenKindWhere
  | TokenKindIf
  | TokenKindThen
  | TokenKindElse
  | TokenKindMatch
  | TokenKindWith
  | TokenKindRecord
  | TokenKindData
  | TokenKindType
  | TokenKindClass
  | TokenKindInstance
  | TokenKindDo
  deriving (Show, Eq, Ord)
