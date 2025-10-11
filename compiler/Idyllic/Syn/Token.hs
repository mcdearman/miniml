module Idyllic.Syn.Token
  ( Token (..),
    TokenKind (..),
    isTrivia,
    isSpace,
    isKeyword,
    isLayoutKeyword,
  )
where

import Data.ByteString (ByteString)
import Idyllic.Utils.Span (Span)

data Token = Token {tokenKind :: TokenKind, tokenSpan :: !Span}
  deriving (Show, Eq, Ord)

data TokenKind
  = TokenKindError
  | TokenKindTab
  | TokenKindNewline
  | TokenKindWhitespace
  | TokenKindComment
  | TokenKindUppercaseIdent ByteString
  | TokenKindLowercaseIdent ByteString
  | TokenKindOpIdent ByteString
  | TokenKindConOpIdent ByteString
  | TokenKindInt Integer
  | TokenKindString ByteString
  | TokenKindChar Char
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
  deriving (Show, Eq, Ord)

isTrivia :: Token -> Bool
isTrivia = go . tokenKind
  where
    go TokenKindWhitespace = True
    go TokenKindNewline = True
    go TokenKindTab = True
    go TokenKindComment = True
    go _ = False

isSpace :: Token -> Bool
isSpace = go . tokenKind
  where
    go TokenKindWhitespace = True
    go _ = False

isKeyword :: Token -> Bool
isKeyword t = case tokenKind t of
  TokenKindLowercaseIdent txt -> go txt
  _ -> False
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
isLayoutKeyword t = case tokenKind t of
  TokenKindLowercaseIdent txt -> go txt
  _ -> False
  where
    go "let" = True
    go "where" = True
    go "do" = True
    go "with" = True
    go _ = False
