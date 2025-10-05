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

data GreenNode = GreenNode
  { greenNodeKind :: !SyntaxKind,
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
  = -- Tokens
    SyntaxKindError
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
  | -- Nodes
    SyntaxKindModule
  | SyntaxKindDecl
  | SyntaxKindImportDecl
  | SyntaxKindRecordDef
  | SyntaxKindClassDef
  | SyntaxKindClassDecl
  | SyntaxKindClassDeclSig
  | SyntaxKindClassDeclBind
  | SyntaxKindInstanceDecl
  | SyntaxKindRecordField
  | SyntaxKindSig
  | SyntaxKindRhs
  | SyntaxKindRhsExpr
  | SyntaxKindRhsGuard
  | SyntaxKindExpr
  | SyntaxKindExprLit
  | SyntaxKindExprIdent
  | SyntaxKindExprApp
  | SyntaxKindExprLam
  | SyntaxKindExprLet
  | SyntaxKindExprDo
  | SyntaxKindExprNeg
  | SyntaxKindExprIf
  | SyntaxKindExprMatch
  | SyntaxKindExprList
  | SyntaxKindExprTuple
  | SyntaxKindExprCons
  | SyntaxKindExprRecord
  | SyntaxKindExprRecordAccess
  | SyntaxKindExprRecordUpdate
  | SyntaxKindExprUnit
  | SyntaxKindRecordLitField
  | SyntaxKindMatchCase
  | SyntaxKindBind
  | SyntaxKindBindFun
  | SyntaxKindBindPat
  | SyntaxKindAlt
  | SyntaxKindGuard
  | SyntaxKindAnno
  | SyntaxKindAnnoVar
  | SyntaxKindAnnoIdent
  | SyntaxKindAnnoFun
  | SyntaxKindAnnoList
  | SyntaxKindAnnoTuple
  | SyntaxKindAnnoRecord
  | SyntaxKindAnnoUnit
  | SyntaxKindPattern
  | SyntaxKindPatternWildcard
  | SyntaxKindPatternLiteral
  | SyntaxKindPatternIdent
  | SyntaxKindPatternTuple
  | SyntaxKindPatternCons
  | SyntaxKindPatternAs
  | SyntaxKindPatternList
  | SyntaxKindPatternRecord
  | SyntaxKindPatternUnit
  | SyntaxKindIdent
  | SyntaxKindLiteral
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
