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
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word16)
import GHC.Exts (Word#)
import MMC.Utils.Unique (Unique)

data GreenNode = GreenNode
  { greenNodeKind :: {-# UNPACK #-} !SyntaxKind,
    greenNodeChildren :: Vector (Either Token GreenNode),
    greenNodeWidth :: {-# UNPACK #-} !Int
  }

instance Show GreenNode where
  show (GreenNode kind children width) =
    "GreenNode { kind = "
      ++ show kind
      ++ ", children = "
      ++ show (U.toList children)
      ++ ", width = "
      ++ show width
      ++ " }"

data Token = Token
  { tokenKind :: {-# UNPACK #-} !SyntaxKind,
    tokenText :: !ByteString
  }
  deriving (Show, Eq, Ord)

newtype SyntaxKind = SyntaxKind Word16 deriving (Show, Eq, Ord)

pattern
  SyntaxKindError,
  SyntaxKindTab,
  SyntaxKindWhitespace,
  SyntaxKindNewline,
  SyntaxKindComment,
  SyntaxKindUppercaseIdent,
  SyntaxKindLowercaseIdent,
  SyntaxKindOpIdent,
  SyntaxKindConOpIdent,
  SyntaxKindInt,
  SyntaxKindString,
  SyntaxKindChar,
  SyntaxKindLParen,
  SyntaxKindRParen,
  SyntaxKindLBrace,
  SyntaxKindRBrace,
  SyntaxKindLBracket,
  SyntaxKindRBracket,
  SyntaxKindBang,
  SyntaxKindHash,
  SyntaxKindBackSlash,
  SyntaxKindColon,
  SyntaxKindSemi,
  SyntaxKindComma,
  SyntaxKindPeriod,
  SyntaxKindEq,
  SyntaxKindLArrow,
  SyntaxKindRArrow,
  SyntaxKindLFatArrow,
  SyntaxKindBar,
  SyntaxKindUnderscore,
  SyntaxKindModule,
  SyntaxKindDecl,
  SyntaxKindImportDecl,
  SyntaxKindRecordDef,
  SyntaxKindClassDef,
  SyntaxKindClassDecl,
  SyntaxKindClassDeclSig,
  SyntaxKindClassDeclBind ::
    SyntaxKind
pattern SyntaxKindError = SyntaxKind 0
pattern SyntaxKindTab = SyntaxKind 1
pattern SyntaxKindWhitespace = SyntaxKind 2
pattern SyntaxKindNewline = SyntaxKind 3
pattern SyntaxKindComment = SyntaxKind 4
pattern SyntaxKindUppercaseIdent = SyntaxKind 5
pattern SyntaxKindLowercaseIdent = SyntaxKind 6
pattern SyntaxKindOpIdent = SyntaxKind 7
pattern SyntaxKindConOpIdent = SyntaxKind 8
pattern SyntaxKindInt = SyntaxKind 9
pattern SyntaxKindString = SyntaxKind 10
pattern SyntaxKindChar = SyntaxKind 11
pattern SyntaxKindLParen = SyntaxKind 12
pattern SyntaxKindRParen = SyntaxKind 13
pattern SyntaxKindLBrace = SyntaxKind 14
pattern SyntaxKindRBrace = SyntaxKind 15
pattern SyntaxKindLBracket = SyntaxKind 16
pattern SyntaxKindRBracket = SyntaxKind 17
pattern SyntaxKindBang = SyntaxKind 18
pattern SyntaxKindHash = SyntaxKind 19
pattern SyntaxKindBackSlash = SyntaxKind 20
pattern SyntaxKindColon = SyntaxKind 21
pattern SyntaxKindSemi = SyntaxKind 22
pattern SyntaxKindComma = SyntaxKind 23
pattern SyntaxKindPeriod = SyntaxKind 24
pattern SyntaxKindEq = SyntaxKind 25
pattern SyntaxKindLArrow = SyntaxKind 26
pattern SyntaxKindRArrow = SyntaxKind 27
pattern SyntaxKindLFatArrow = SyntaxKind 28
pattern SyntaxKindBar = SyntaxKind 29
pattern SyntaxKindUnderscore = SyntaxKind 30
pattern SyntaxKindModule = SyntaxKind 31
pattern SyntaxKindDecl = SyntaxKind 32
pattern SyntaxKindImportDecl = SyntaxKind 33
pattern SyntaxKindRecordDef = SyntaxKind 34
pattern SyntaxKindClassDef = SyntaxKind 35
pattern SyntaxKindClassDecl = SyntaxKind 36
pattern SyntaxKindClassDeclSig = SyntaxKind 37
pattern SyntaxKindClassDeclBind = SyntaxKind 38

-- data SyntaxKind
--   = -- Tokens
--     SyntaxKindError
--   | SyntaxKindTab
--   | SyntaxKindWhitespace
--   | SyntaxKindNewline
--   | SyntaxKindComment
--   | SyntaxKindUppercaseIdent
--   | SyntaxKindLowercaseIdent
--   | SyntaxKindOpIdent
--   | SyntaxKindConOpIdent
--   | SyntaxKindInt
--   | SyntaxKindString
--   | SyntaxKindChar
--   | SyntaxKindLParen
--   | SyntaxKindRParen
--   | SyntaxKindLBrace
--   | SyntaxKindRBrace
--   | SyntaxKindLBracket
--   | SyntaxKindRBracket
--   | SyntaxKindBang
--   | SyntaxKindHash
--   | SyntaxKindBackSlash
--   | SyntaxKindColon
--   | SyntaxKindSemi
--   | SyntaxKindComma
--   | SyntaxKindPeriod
--   | SyntaxKindEq
--   | SyntaxKindLArrow
--   | SyntaxKindRArrow
--   | SyntaxKindLFatArrow
--   | SyntaxKindBar
--   | SyntaxKindUnderscore
--   | -- Nodes
--     SyntaxKindModule
--   | SyntaxKindDecl
--   | SyntaxKindImportDecl
--   | SyntaxKindRecordDef
--   | SyntaxKindClassDef
--   | SyntaxKindClassDecl
--   | SyntaxKindClassDeclSig
--   | SyntaxKindClassDeclBind
--   | SyntaxKindInstanceDecl
--   | SyntaxKindRecordField
--   | SyntaxKindSig
--   | SyntaxKindRhs
--   | SyntaxKindRhsExpr
--   | SyntaxKindRhsGuard
--   | SyntaxKindExpr
--   | SyntaxKindExprLit
--   | SyntaxKindExprIdent
--   | SyntaxKindExprApp
--   | SyntaxKindExprLam
--   | SyntaxKindExprLet
--   | SyntaxKindExprDo
--   | SyntaxKindExprNeg
--   | SyntaxKindExprIf
--   | SyntaxKindExprMatch
--   | SyntaxKindExprList
--   | SyntaxKindExprTuple
--   | SyntaxKindExprCons
--   | SyntaxKindExprRecord
--   | SyntaxKindExprRecordAccess
--   | SyntaxKindExprRecordUpdate
--   | SyntaxKindExprUnit
--   | SyntaxKindRecordLitField
--   | SyntaxKindMatchCase
--   | SyntaxKindBind
--   | SyntaxKindBindFun
--   | SyntaxKindBindPat
--   | SyntaxKindAlt
--   | SyntaxKindGuard
--   | SyntaxKindAnno
--   | SyntaxKindAnnoVar
--   | SyntaxKindAnnoIdent
--   | SyntaxKindAnnoFun
--   | SyntaxKindAnnoList
--   | SyntaxKindAnnoTuple
--   | SyntaxKindAnnoRecord
--   | SyntaxKindAnnoUnit
--   | SyntaxKindPattern
--   | SyntaxKindPatternWildcard
--   | SyntaxKindPatternLiteral
--   | SyntaxKindPatternIdent
--   | SyntaxKindPatternTuple
--   | SyntaxKindPatternCons
--   | SyntaxKindPatternAs
--   | SyntaxKindPatternList
--   | SyntaxKindPatternRecord
--   | SyntaxKindPatternUnit
--   | SyntaxKindIdent
--   | SyntaxKindLiteral
--   deriving (Show, Eq, Ord)

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
