{-# LANGUAGE TypeFamilies #-}

module MMC.Lexer (tokenize, WithPos (..)) where

import Control.Applicative (empty, (<|>))
import Control.Monad.Combinators (manyTill_)
import Data.Data (Proxy (Proxy))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text, pack, unpack)
import Data.Void (Void)
import MMC.Common (Span (..))
import MMC.Token
import Text.Megaparsec
  ( MonadParsec (eof, notFollowedBy, try, withRecovery),
    ParseError,
    ParseErrorBundle,
    Parsec,
    PosState (..),
    SourcePos (sourceLine),
    choice,
    getOffset,
    getSourcePos,
    many,
    manyTill,
    parse,
  )
import Text.Megaparsec.Char
  ( alphaNumChar,
    char,
    char',
    lowerChar,
    space1,
    string,
    upperChar,
  )
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Stream hiding (Token)
import qualified Text.Megaparsec.Stream as S
import Prelude hiding (span)

data WithPos a = WithPos
  { wpStart :: SourcePos,
    wpEnd :: SourcePos,
    wpSpan :: Span,
    wpLen :: Int,
    wpVal :: a
  }
  deriving (Show, Eq, Ord)

type Lexer = Parsec Void Text

withPos :: Lexer a -> Lexer (WithPos a)
withPos p = do
  startPos <- getSourcePos
  startOffset <- getOffset
  result <- p
  endPos <- getSourcePos
  endOffset <- getOffset
  return $ WithPos startPos endPos (Span startOffset endOffset) (endOffset - startOffset) result

lexemeWithPos :: Lexer a -> Lexer (WithPos a)
lexemeWithPos p = withPos p <* sc

sc :: Lexer ()
sc = L.space space1 (L.skipLineComment "--") empty

octal :: Lexer Integer
octal = char '0' >> char' 'o' >> L.octal

hexadecimal :: Lexer Integer
hexadecimal = char '0' >> char' 'x' >> L.hexadecimal

int :: Lexer Token
int = TokInt <$> (try octal <|> try hexadecimal <|> L.decimal)

bool :: Lexer Token
bool = TokBool <$> choice [True <$ string "true", False <$ string "false"]

str :: Lexer Token
str = TokString <$> (char '\"' *> (pack <$> manyTill L.charLiteral (char '\"')))

ident :: Lexer Token
ident = try $ do
  name <- pack <$> ((:) <$> identStartChar <*> many identChar)
  if name `elem` keywords
    then fail $ "keyword " ++ unpack name ++ " cannot be an identifier"
    else return $ TokLowerCaseIdent name
  where
    identStartChar = lowerChar <|> char '_'
    identChar = alphaNumChar <|> char '_' <|> char '\''

    keywords :: [Text]
    keywords =
      [ "module",
        "import",
        "as",
        "pub",
        "def",
        "let",
        "in",
        "where",
        "if",
        "then",
        "else",
        "match",
        "with",
        "record",
        "data",
        "type",
        "class",
        "impl",
        "do"
      ]

upperCaseIdent :: Lexer Token
upperCaseIdent = TokUpperCaseIdent . pack <$> ((:) <$> upperChar <*> many alphaNumChar)

token :: Lexer (WithPos Token)
token =
  lexemeWithPos $
    choice
      [ TokComment <$ string "--" <* manyTill_ L.charLiteral (char '\n'),
        int,
        bool,
        str,
        upperCaseIdent,
        ident,
        TokLParen <$ char '(',
        TokRParen <$ char ')',
        TokLBrace <$ char '{',
        TokRBrace <$ char '}',
        TokLBracket <$ char '[',
        TokRBracket <$ char ']',
        TokPlus <$ char '+',
        TokArrow <$ string "->",
        TokMinus <$ char '-',
        TokStar <$ char '*',
        TokSlash <$ char '/',
        TokBackSlash <$ char '\\',
        TokPercent <$ char '%',
        TokColon <$ char ':',
        TokSemi <$ char ';',
        TokComma <$ char ',',
        TokEq <$ char '=',
        TokBar <$ char '|',
        TokUnderscore <$ char '_',
        TokLet <$ string "let",
        TokIn <$ string "in",
        TokIf <$ string "if",
        TokThen <$ string "then",
        TokElse <$ string "else",
        TokMatch <$ string "match",
        TokWith <$ string "with"
      ]

tokenize :: Text -> Either (ParseErrorBundle Text Void) [WithPos Token]
tokenize src = parse (many token) "" src
