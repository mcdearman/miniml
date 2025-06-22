module MMC.Token (Token (..), LToken) where

import Data.Text (Text)
import MMC.Common (Located)

type LToken = Located Token

data Token
  = TokError
  | TokEOF
  | TokWhitespace Int
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
  | TokPlus
  | TokMinus
  | TokStar
  | TokSlash
  | TokBackSlash
  | TokPercent
  | TokColon
  | TokSemi
  | TokComma
  | TokPeriod
  | TokEq
  | TokRArrow
  | TokLArrow
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
