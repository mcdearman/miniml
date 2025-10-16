module Idyllic.Syn.Parser where

import Data.Text (Text, pack, unpack)
import Data.Void
import Idyllic.Syn.AST
import Idyllic.Utils.Span (Span (..), Spanned (..))
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

parseTerm :: Text -> Either (ParseErrorBundle Text Void) Expr
parseTerm = runParser (sc *> exprParser <* eof) ""

exprParser :: Parser Expr
exprParser = app
  where
    int = ExprInt . fromInteger <$> lexeme L.decimal
    var = ExprVar <$> ident
    atom = int <|> let' <|> var <|> lam <|> parens exprParser
    let' = try $ ExprLet <$> (symbol "let" *> bind) <*> (symbol "in" *> exprParser)
    lam = symbol "\\" *> (flip (foldr ExprLam) <$> some ident <*> (symbol "->" *> exprParser))
    app = foldl1 ExprApp <$> some atom

bind :: Parser Bind
bind = funBind <|> nameBind
  where
    nameBind = BindName <$> (ident <* symbol "=") <*> exprParser
    funBind = try $ BindFun <$> ident <*> some ident <* symbol "=" <*> exprParser

parens :: Parser a -> Parser a
parens = lexeme <$> between (char '(') (char ')')

ident :: Parser Text
ident = try $ lexeme $ do
  name <- pack <$> ((:) <$> identStartChar <*> many identChar) <* sc
  if name `elem` keywords
    then fail $ "keyword " ++ unpack name ++ " cannot be used in place of identifier"
    else pure name
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

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

sc :: Parser ()
sc = L.space space1 lineComment empty

lineComment :: Parser ()
lineComment = L.skipLineComment "--"

withSpan :: Parser a -> Parser (Spanned a)
withSpan p = do
  start <- getOffset
  x <- p
  Spanned x . Span start <$> getOffset