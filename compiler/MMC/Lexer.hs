module MMC.Lexer (tokenize) where

import Control.Applicative (empty, liftA, optional, (<|>))
import Control.Monad (void)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Void (Void)
import MMC.Common (Loc (..), Located (..), unLoc)
import MMC.Token (LToken, Token (..))
import Text.Megaparsec
  ( MonadParsec (eof, getParserState, lookAhead, notFollowedBy, takeWhile1P, takeWhileP, token, try),
    ParseErrorBundle,
    Parsec,
    choice,
    getOffset,
    many,
    manyTill,
    oneOf,
    option,
    parse,
    some,
    (<?>),
  )
import Text.Megaparsec.Char (alphaNumChar, char, char', lowerChar, space1, string, upperChar)
import qualified Text.Megaparsec.Char.Lexer as L

type Lexer = Parsec Void Text

tokenize :: Text -> Either (ParseErrorBundle Text Void) [LToken]
tokenize src = (parse . many . withLoc) tokenL "" src

tokenL :: Lexer Token
tokenL =
  choice
    [ lineComment *> tokenL,
      newline,
      whitespace,
      upperCaseIdent,
      lowerCaseIdent,
      conOpIdent,
      opIdent,
      int,
      str,
      charT,
      TokModule <$ string "module" <?> "module",
      TokImport <$ string "import" <?> "import",
      TokAs <$ string "as" <?> "as",
      TokLet <$ string "let" <?> "let",
      TokIn <$ string "in" <?> "in",
      TokWhere <$ string "where" <?> "where",
      TokIf <$ string "if" <?> "if",
      TokThen <$ string "then" <?> "then",
      TokElse <$ string "else" <?> "else",
      TokMatch <$ string "match" <?> "match",
      TokWith <$ string "with" <?> "with",
      TokRecord <$ string "record" <?> "record",
      TokData <$ string "data" <?> "data",
      TokType <$ string "type" <?> "type",
      TokClass <$ string "class" <?> "class",
      TokInstance <$ string "instance" <?> "instance",
      TokDo <$ string "do" <?> "do",
      TokLParen <$ char '(',
      TokRParen <$ char ')',
      TokLBrace <$ char '{',
      TokRBrace <$ char '}',
      TokLBracket <$ char '[',
      TokRBracket <$ char ']',
      TokBang <$ char '!',
      TokPlus <$ char '+',
      TokLArrow <$ string "->",
      TokMinus <$ char '-',
      TokStar <$ char '*',
      TokSlash <$ char '/',
      TokBackSlash <$ char '\\',
      TokPercent <$ char '%',
      TokColon <$ char ':',
      TokSemi <$ char ';',
      TokComma <$ char ',',
      TokPeriod <$ char '.',
      TokLFatArrow <$ string "=>",
      TokEq <$ char '=',
      TokRArrow <$ string "<-",
      TokBar <$ char '|',
      TokUnderscore <$ char '_'
    ]

{-# INLINEABLE lowerCaseIdent #-}
lowerCaseIdent :: Lexer Token
lowerCaseIdent = try $ do
  name <- pack <$> ((:) <$> identStartChar <*> many identChar)
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
upperCaseIdent = TokUpperCaseIdent <$> (pack <$> ((:) <$> upperChar <*> many alphaNumChar))

{-# INLINEABLE opIdent #-}
opIdent :: Lexer Token
opIdent = try $ do
  sym <- choice [startSpecial, startNotEq] <* notFollowedBy (oneOf ['=', '.', '@', '|', ':', '-'])
  if sym `elem` reservedSymbols
    then fail $ "symbol " ++ unpack sym ++ " cannot be used in place of identifier"
    else pure $ TokOpIdent sym
  where
    opStartChar = oneOf ("!$%&*+/<>?~" ++ ['^' .. '`'] :: String)
    startSpecial = try $ T.cons <$> oneOf ['=', '.', '@', '|', ':'] <*> takeWhile1P Nothing isOpChar
    startNotEq = T.cons <$> opStartChar <*> takeWhileP Nothing isOpChar

    reservedSymbols :: [Text]
    reservedSymbols =
      [ "->",
        "=>",
        "<-",
        "!"
      ]

{-# INLINEABLE conOpIdent #-}
conOpIdent :: Lexer Token
conOpIdent = try $ TokConOpIdent <$> (T.cons <$> char ':' <*> takeWhile1P Nothing isOpChar)

{-# INLINEABLE isOpChar #-}
isOpChar :: Char -> Bool
isOpChar c = c `elem` ("!$%&*+./<=>?@|\\~:" ++ ['^' .. '`'] :: String)

newline :: Lexer Token
newline = try $ TokNewline <$ oneOf ['\n', '\r']

scn :: Lexer ()
scn = L.space (void $ oneOf ['\n', '\r']) lineComment empty

sc :: Lexer ()
sc = L.space space1 lineComment empty

whitespace :: Lexer Token
whitespace = TokWhitespace <$> (T.length <$> takeWhile1P Nothing isSpace)
  where
    isSpace ' ' = True
    isSpace '\t' = True
    isSpace _ = False

lineComment :: Lexer ()
lineComment = L.skipLineComment "--"

{-# INLINE octal #-}
octal :: Lexer Integer
octal = try $ char '0' *> char' 'o' *> L.octal

{-# INLINE hexadecimal #-}
hexadecimal :: Lexer Integer
hexadecimal = try $ char '0' *> char' 'x' *> L.hexadecimal

{-# INLINE int #-}
int :: Lexer Token
int = TokInt <$> choice [octal, hexadecimal, L.decimal]

{-# INLINE str #-}
str :: Lexer Token
str = TokString <$> (char '\"' *> (pack <$> manyTill L.charLiteral (char '\"')))

{-# INLINE charT #-}
charT :: Lexer Token
charT = TokChar <$> (char '\'' *> L.charLiteral <* char '\'')

{-# INLINE withLoc #-}
withLoc :: Lexer a -> Lexer (Located a)
withLoc p = do
  start <- getOffset
  x <- p
  end <- getOffset
  pure $ Located x (Loc start end)