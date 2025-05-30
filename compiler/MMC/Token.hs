module MMC.Token (Token (..), pShowToken) where

import Data.Text (Text)

data Token
  = TokError
  | TokEOF
  | TokWhitespace
  | TokComment
  | TokUpperCaseIdent Text
  | TokLowerCaseIdent Text
  | TokOpIdent Text
  | TokConOpIdent Text
  | TokInt Integer
  | TokBool Bool
  | TokString Text
  | TokChar Char
  | TokBang
  | TokLParen
  | TokRParen
  | TokLBrace
  | TokRBrace
  | TokLBracket
  | TokRBracket
  | TokPlus
  | TokMinus
  | TokStar
  | TokSlash
  | TokBackSlash
  | TokPercent
  | TokColon
  | TokSemi
  | TokComma
  | TokEq
  | TokArrow
  | TokFatArrow
  | TokBar
  | TokUnderscore
  | TokRecord
  | TokData
  | TokLet
  | TokIn
  | TokIf
  | TokThen
  | TokElse
  | TokMatch
  | TokWith
  deriving (Show, Eq, Ord)

pShowToken :: Token -> String
pShowToken TokError = "Error"
pShowToken TokEOF = "EOF"
pShowToken TokWhitespace = "Whitespace"
pShowToken TokComment = "Comment"
pShowToken (TokUpperCaseIdent x) = "UpperCaseIdent " ++ show x
pShowToken (TokLowerCaseIdent x) = "LowerCaseIdent " ++ show x
pShowToken (TokOpIdent x) = "OpIdent " ++ show x
pShowToken (TokConOpIdent x) = "ConOpIdent " ++ show x
pShowToken (TokInt x) = "Int" ++ show x
pShowToken (TokBool x) = "Bool" ++ show x
pShowToken (TokString x) = "String" ++ show x
pShowToken (TokChar x) = "Char" ++ show x
pShowToken TokBang = "Bang"
pShowToken TokLParen = "LParen"
pShowToken TokRParen = "RParen"
pShowToken TokLBrace = "LBrace"
pShowToken TokRBrace = "RBrace"
pShowToken TokLBracket = "LBracket"
pShowToken TokRBracket = "RBracket"
pShowToken TokPlus = "Plus"
pShowToken TokMinus = "Minus"
pShowToken TokStar = "Star"
pShowToken TokSlash = "Slash"
pShowToken TokBackSlash = "BackSlash"
pShowToken TokPercent = "Percent"
pShowToken TokColon = "Colon"
pShowToken TokSemi = "Semi"
pShowToken TokComma = "Comma"
pShowToken TokEq = "Assign"
pShowToken TokArrow = "Arrow"
pShowToken TokFatArrow = "FatArrow"
pShowToken TokBar = "Bar"
pShowToken TokUnderscore = "Underscore"
pShowToken TokRecord = "Record"
pShowToken TokData = "Data"
pShowToken TokLet = "Let"
pShowToken TokIn = "In"
pShowToken TokIf = "If"
pShowToken TokThen = "Then"
pShowToken TokElse = "Else"
pShowToken TokMatch = "Match"
pShowToken TokWith = "With"