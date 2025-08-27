module MMC.Syn.Token (Token (..)) where

import Data.Text (Text)
import MMC.Common (Loc)

data Token = Token
  { tokenKind :: TokenKind,
    tokenLoc :: Loc
  }
  deriving (Show, Eq, Ord)

data TokenKind
  = TokError
  | TokNewline
  | TokComment
  | TokUpperCaseIdent Text
  | TokLowerCaseIdent Text
  | TokOpIdent Text
  | TokConOpIdent Text
  | TokInt Integer
  | TokString Text
  | TokChar Char
  | TokLParen
  | TokRParen
  | TokLBrace
  | TokRBrace
  | TokLBracket
  | TokRBracket
  | TokBang
  | TokBackSlash
  | TokColon
  | TokSemi
  | TokComma
  | TokPeriod
  | TokEq
  | TokLArrow
  | TokRArrow
  | TokLFatArrow
  | TokBar
  | TokUnderscore
  | TokModule
  | TokImport
  | TokAs
  | TokLet
  | TokIn
  | TokWhere
  | TokIf
  | TokThen
  | TokElse
  | TokMatch
  | TokWith
  | TokRecord
  | TokData
  | TokType
  | TokClass
  | TokInstance
  | TokDo
  deriving (Show, Eq, Ord)