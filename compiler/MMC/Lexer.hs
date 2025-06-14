module MMC.Lexer (tokenize) where

import Control.Applicative (empty, optional, (<|>))
import Control.Monad (void)
import Data.Text (Text, pack, unpack)
import Data.Void (Void)
import MMC.Common (Loc (..), Located (..), unLoc)
import MMC.Token (LToken, Token (..))
import Text.Megaparsec
  ( MonadParsec (eof, getParserState, lookAhead, notFollowedBy, takeWhile1P, token, try),
    ParseErrorBundle,
    Parsec,
    choice,
    getOffset,
    many,
    manyTill,
    option,
    parse,
    satisfy,
    some,
    (<?>),
  )
import Text.Megaparsec.Char (alphaNumChar, char, char', lowerChar, space1, string, upperChar)
import qualified Text.Megaparsec.Char.Lexer as L

type Lexer = Parsec Void Text

tokenize :: Text -> Either (ParseErrorBundle Text Void) [LToken]
tokenize src = parse (many tokenL) "" src

tokenL :: Lexer LToken
tokenL =
  withLoc $
    choice
      [ TokModule <$ symbol "module" <?> "module",
        TokImport <$ symbol "import" <?> "import",
        TokAs <$ symbol "as" <?> "as",
        TokLet <$ symbol "let" <?> "let",
        TokIn <$ symbol "in" <?> "in",
        TokWhere <$ symbol "where" <?> "where",
        TokIf <$ symbol "if" <?> "if",
        TokThen <$ symbol "then" <?> "then",
        TokElse <$ symbol "else" <?> "else",
        TokMatch <$ symbol "match" <?> "match",
        TokWith <$ symbol "with" <?> "with",
        TokRecord <$ symbol "record" <?> "record",
        TokData <$ symbol "data" <?> "data",
        TokType <$ symbol "type" <?> "type",
        TokClass <$ symbol "class" <?> "class",
        TokInstance <$ symbol "instance" <?> "instance",
        TokDo <$ symbol "do" <?> "do",
        int,
        str
      ]

-- {-# INLINEABLE lowerCaseIdent #-}
lowerCaseIdent :: Lexer Token
lowerCaseIdent = try $ do
  name <- pack <$> ((:) <$> identStartChar <*> many identChar) <* sc
  if name `elem` keywords
    then fail $ "keyword " ++ unpack name ++ " cannot be used in place of identifier"
    else pure $ TokLowerCaseIdent name
  where
    identStartChar = lowerChar <|> char '_'
    identChar = alphaNumChar <|> char '_' <|> char '\''

    keywords :: [Text]
    keywords =
      [ "module",
        "import",
        "as",
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
        "instance",
        "do"
      ]

{-# INLINEABLE upperCaseIdent #-}
upperCaseIdent :: Lexer Token
upperCaseIdent = TokUpperCaseIdent <$> (pack <$> ((:) <$> upperChar <*> many alphaNumChar)) <* sc

-- {-# INLINE sc #-}
sc :: Lexer ()
sc = L.space (void $ some (char ' ' <|> char '\t')) lineComment empty

lineComment :: Lexer ()
lineComment = L.skipLineComment "--"

{-# INLINE lexeme #-}
lexeme :: Lexer a -> Lexer a
lexeme = L.lexeme sc

charL :: Char -> Lexer Char
charL c = lexeme (char c)

{-# INLINE symbol #-}
symbol :: Text -> Lexer Text
symbol = L.symbol sc

{-# INLINE octal #-}
octal :: Lexer Integer
octal = try $ char '0' *> char' 'o' *> L.octal

{-# INLINE hexadecimal #-}
hexadecimal :: Lexer Integer
hexadecimal = try $ char '0' *> char' 'x' *> L.hexadecimal

{-# INLINE int #-}
int :: Lexer Token
int = TokInt <$> lexeme (choice [octal, hexadecimal, L.decimal])

{-# INLINE str #-}
str :: Lexer Token
str = TokString <$> lexeme (char '\"' *> (pack <$> manyTill L.charLiteral (char '\"')))

{-# INLINE withLoc #-}
withLoc :: Lexer a -> Lexer (Located a)
withLoc p = do
  start <- getOffset
  x <- p
  end <- getOffset
  pure $ Located x (Loc start end)