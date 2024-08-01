module Parser where

import AST
import Control.Applicative (empty, (<|>))
import Data.Text (Text, pack)
import Data.Void
import Text.Megaparsec
  ( MonadParsec (notFollowedBy, try),
    ParseErrorBundle,
    Parsec,
    between,
    choice,
    many,
    manyTill,
    parse,
    satisfy,
  )
import Text.Megaparsec.Char
  ( alphaNumChar,
    char,
    letterChar,
    space1,
  )
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

int :: Parser Integer
int = lexeme L.decimal

signedInt :: Parser Integer
signedInt = lexeme $ L.signed (notFollowedBy space1) int