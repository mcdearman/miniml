module MMC.Parser where

import Control.Applicative (empty, optional, (<|>))
-- import MMC.Token (Token (..))

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Array (Array, listArray)
import Data.Functor (($>))
import qualified Data.Set as Set
import Data.Text (Text, pack, unpack)
import Data.Void
import Data.Word (Word8)
import MMC.AST
import MMC.Common
import Text.Megaparsec
  ( MonadParsec (eof, getParserState, lookAhead, notFollowedBy, takeWhile1P, token, try),
    ParseErrorBundle,
    Parsec,
    State (stateInput),
    Stream (take1_),
    between,
    choice,
    getInput,
    getOffset,
    many,
    manyTill,
    option,
    parse,
    satisfy,
    sepBy1,
    sepEndBy,
    sepEndBy1,
    some,
    (<?>),
  )
import Text.Megaparsec.Char (alphaNumChar, char, char', lowerChar, space1, string, upperChar)
import Text.Megaparsec.Char.Lexer (indentBlock)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug (MonadParsecDbg (dbg))
import Prelude hiding (span)

type Parser = Parsec Void Text

parseStream :: Text -> Either (ParseErrorBundle Text Void) Prog
parseStream = Text.Megaparsec.parse repl ""

repl :: Parser Prog
repl = undefined

def :: Parser Def
def = undefined

expr :: Parser Expr
expr = makeExprParser apply operatorTable
  where
    unit' = unit $> Unit
    litExpr = Lit <$> lit
    varExpr = Var <$> lowerCaseIdent

    simple :: Parser ExprSort
    simple = choice [litExpr, varExpr, unit', parens (spannedVal <$> expr)]

    lambda :: Parser ExprSort
    lambda = Lam <$> (char '\\' *> some pattern') <* symbol "->" <*> expr

    let' :: Parser ExprSort
    let' = Let <$> (kwLet *> pattern') <* char '=' <*> expr <* symbol "in" <*> expr

    fun :: Parser ExprSort
    fun =
      try (Fun <$> (let' *> lowerCaseIdent) <*> some pattern')
        <* symbol "->"
        <*> expr
        <* kwIn
        <*> expr

    if' :: Parser ExprSort
    if' = If <$> (kwIf *> expr) <* kwThen <*> expr <* kwElse <*> expr

    match :: Parser ExprSort
    match =
      indentBlock scn $ do
        kwMatch
        scrut <- expr
        kwWith
        pure $ L.IndentSome Nothing (cont scrut) alt
      where
        alt :: Parser (Pattern, Expr)
        alt = ((,) <$> pattern' <*> (symbol "->" *> expr))

        cont :: Expr -> [(Pattern, Expr)] -> Parser ExprSort
        cont scr alts = pure $ Match scr alts

    list :: Parser ExprSort
    list = List <$> brackets (expr `sepEndBy` char ',')

    tuple :: Parser ExprSort
    tuple = Tuple <$> try (parens ((:) <$> (expr <* char ',')) <*> expr `sepEndBy1` char ',')

    record :: Parser ExprSort
    record =
      Record
        <$> optional upperCaseIdent
        <*> braces (((,) <$> lowerCaseIdent <*> (char '=' *> expr)) `sepEndBy1` char ',')

    atom :: Parser Expr
    atom =
      withSpan $
        choice
          [ fun,
            let',
            lambda,
            if',
            match,
            list,
            tuple <|> simple,
            record
          ]

    apply :: Parser Expr
    apply = do
      fargs <- some atom
      pure $ foldl1 (\f a -> Spanned (App f a) (span f <> span a)) fargs

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [prefix "-" (\s e -> Spanned (Unary (Spanned UnOpNeg s) e) (s <> span e))],
    [ binary "*" (\s l r -> Spanned (Binary (Spanned BinOpMul s) l r) (span l <> span r)),
      binary "/" (\s l r -> Spanned (Binary (Spanned BinOpDiv s) l r) (span l <> span r)),
      binary "%" (\s l r -> Spanned (Binary (Spanned BinOpMod s) l r) (span l <> span r))
    ],
    [ binary "+" (\s l r -> Spanned (Binary (Spanned BinOpAdd s) l r) (span l <> span r)),
      binary "-" (\s l r -> Spanned (Binary (Spanned BinOpSub s) l r) (span l <> span r))
    ]
  ]

prefix, postfix :: Text -> (Span -> Expr -> Expr) -> Operator Parser Expr
prefix name f = Prefix (f . span <$> withSpan (string name))
postfix name f = Postfix (f . span <$> withSpan (string name))

typeAnno :: Parser TypeAnno
typeAnno = arrowType <|> baseType
  where
    arrowType :: Parser TypeAnno
    arrowType = withSpan $ try (TypeAnnoFun <$> baseType <* symbol "->") <*> typeAnno

    baseType :: Parser TypeAnno
    baseType =
      withSpan $
        choice
          [ varType,
            identType,
            listType,
            unit $> TypeAnnoUnit
              <|> tupleType
              <|> parens (spannedVal <$> typeAnno)
          ]

    varType = TypeAnnoVar <$> lowerCaseIdent
    identType = TypeAnnoIdent <$> upperCaseIdent
    listType = TypeAnnoList <$> brackets typeAnno
    tupleType =
      TypeAnnoTuple
        <$> try (parens ((:) <$> (typeAnno <* char ',')) <*> typeAnno `sepEndBy1` char ',')

pattern' :: Parser Pattern
pattern' = withSpan $ choice [wildcard, litP, identP, consP, listP, unitP]
  where
    wildcard = char '_' $> PatternWildcard
    litP = PatternLit <$> lit
    identP = PatternIdent <$> lowerCaseIdent
    consP = PatternCons <$> (upperCaseIdent <* char '@') <*> some pattern'
    listP = PatternList <$> brackets (pattern' `sepEndBy` char ',')
    unitP = unit $> PatternUnit

parens :: Parser a -> Parser a
parens = between (lexeme $ char '(') (lexeme $ char ')')

brackets :: Parser a -> Parser a
brackets = between (lexeme $ char '[') (lexeme $ char ']')

braces :: Parser a -> Parser a
braces = between (lexeme $ char '{') (lexeme $ char '}')

binary :: Text -> (Span -> Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f . span <$> withSpan (symbol name))

{-# INLINEABLE lowerCaseIdent #-}
lowerCaseIdent :: Parser Ident
lowerCaseIdent = try $ do
  name <- withSpan $ pack <$> ((:) <$> identStartChar <*> many identChar)
  let !sv = spannedVal name
  if sv `elem` keywords
    then fail $ "keyword " ++ unpack sv ++ " cannot be an identifier"
    else pure $ Ident name
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
upperCaseIdent :: Parser Ident
upperCaseIdent = Ident <$> (withSpan $ pack <$> ((:) <$> upperChar <*> many alphaNumChar))

scn :: Parser ()
scn = L.space space1 lineComment empty

{-# INLINE sc #-}
sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) lineComment empty

lineComment :: Parser ()
lineComment = L.skipLineComment "--"

{-# INLINE lexeme #-}
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

{-# INLINE symbol #-}
symbol :: Text -> Parser Text
symbol = L.symbol sc

{-# INLINE octal #-}
octal :: Parser Integer
octal = try (char '0' *> char' 'o') *> L.octal

{-# INLINE hexadecimal #-}
hexadecimal :: Parser Integer
hexadecimal = try (char '0' >> char' 'x') >> L.hexadecimal

{-# INLINE int #-}
int :: Parser Integer
int = lexeme $ choice [octal, hexadecimal, L.decimal]

{-# INLINE bool #-}
bool :: Parser Bool
bool = lexeme $ choice [True <$ string "true", False <$ string "false"]

{-# INLINE str #-}
str :: Parser Text
str = lexeme $ char '\"' *> (pack <$> manyTill L.charLiteral (char '\"'))

{-# INLINE unit #-}
unit :: Parser ()
unit = symbol "()" $> ()

{-# INLINE lit #-}
lit :: Parser Lit
lit =
  choice
    [ Int <$> int,
      Bool <$> bool,
      String <$> str
    ]

{-# INLINE kwModule #-}
kwModule :: Parser ()
kwModule = symbol "module" $> () <?> "module"

{-# INLINE kwImport #-}
kwImport :: Parser ()
kwImport = symbol "import" $> () <?> "import"

{-# INLINE kwAs #-}
kwAs :: Parser ()
kwAs = symbol "as" $> () <?> "as"

{-# INLINE kwLet #-}
kwLet :: Parser ()
kwLet = symbol "let" $> () <?> "let"

{-# INLINE kwIn #-}
kwIn :: Parser ()
kwIn = symbol "in" $> () <?> "in"

{-# INLINE kwWhere #-}
kwWhere :: Parser ()
kwWhere = string "where" $> () <?> "where"

{-# INLINE kwIf #-}
kwIf :: Parser ()
kwIf = symbol "if" $> () <?> "if"

{-# INLINE kwThen #-}
kwThen :: Parser ()
kwThen = symbol "then" $> () <?> "then"

{-# INLINE kwElse #-}
kwElse :: Parser ()
kwElse = symbol "else" $> () <?> "else"

{-# INLINE kwMatch #-}
kwMatch :: Parser ()
kwMatch = symbol "match" $> () <?> "match"

{-# INLINE kwWith #-}
kwWith :: Parser ()
kwWith = string "with" $> () <?> "with"

{-# INLINE kwRecord #-}
kwRecord :: Parser ()
kwRecord = symbol "record" $> () <?> "record"

{-# INLINE kwData #-}
kwData :: Parser ()
kwData = symbol "data" $> () <?> "data"

{-# INLINE kwType #-}
kwType :: Parser ()
kwType = symbol "type" $> () <?> "type"

{-# INLINE kwClass #-}
kwClass :: Parser ()
kwClass = symbol "class" $> () <?> "class"

{-# INLINE kwInstance #-}
kwInstance :: Parser ()
kwInstance = symbol "impl" $> () <?> "impl"

{-# INLINE kwDo #-}
kwDo :: Parser ()
kwDo = string "do" $> () <?> "do"

{-# INLINE withSpan #-}
withSpan :: Parser a -> Parser (Spanned a)
withSpan p = do
  start <- getOffset
  x <- p
  end <- getOffset
  pure $ Spanned x (Span start end)