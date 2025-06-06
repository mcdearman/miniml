module MMC.Parser (parse') where

import Control.Applicative (empty, optional, (<|>))
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
import Prelude hiding (getLoc)

type Parser = Parsec Void Text

parse' :: InputMode -> Text -> Either (ParseErrorBundle Text Void) Prog
parse' InputModeFile = Text.Megaparsec.parse module' ""
parse' InputModeInteractive = Text.Megaparsec.parse repl ""

module' :: Parser Prog
module' = undefined

repl :: Parser Prog
repl = undefined

def :: Parser Def
def = undefined

expr :: Parser LExpr
expr = makeExprParser apply operatorTable
  where
    unit' = unit $> Unit
    litExpr = Lit <$> lit
    varExpr = Var <$> lowerCaseIdent

    simple :: Parser Expr
    simple = choice [litExpr, varExpr, unit', parens (unLoc <$> expr)]

    lambda :: Parser Expr
    lambda = Lam <$> (char '\\' *> some pattern') <* symbol "->" <*> expr

    let' :: Parser Expr
    let' = Let <$> (kwLet *> pattern') <* char '=' <*> expr <* symbol "in" <*> expr

    fun :: Parser Expr
    fun =
      try (Let <$> (let' *> lowerCaseIdent) <*> some pattern')
        <* symbol "->"
        <*> expr
        <* kwIn
        <*> expr

    if' :: Parser Expr
    if' = If <$> (kwIf *> expr) <* kwThen <*> expr <* kwElse <*> expr

    match :: Parser Expr
    match =
      indentBlock scn $ do
        kwMatch
        scrut <- expr
        kwWith
        pure $ L.IndentSome Nothing (cont scrut) alt
      where
        alt :: Parser (LPattern, LExpr)
        alt = ((,) <$> pattern' <*> (symbol "->" *> expr))

        cont :: LExpr -> [(LPattern, LExpr)] -> Parser Expr
        cont scr alts = pure $ Match scr alts

    list :: Parser Expr
    list = List <$> brackets (expr `sepEndBy` char ',')

    tuple :: Parser Expr
    tuple = Tuple <$> try (parens ((:) <$> (expr <* char ',')) <*> expr `sepEndBy1` char ',')

    record :: Parser Expr
    record =
      Record
        <$> optional upperCaseIdent
        <*> braces (((,) <$> lowerCaseIdent <*> (char '=' *> expr)) `sepEndBy1` char ',')

    atom :: Parser LExpr
    atom =
      withLoc $
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

    apply :: Parser LExpr
    apply = do
      fargs <- some atom
      pure $ foldl1 (\f a -> Located (App f a) (getLoc f <> getLoc a)) fargs

bind :: Parser Bind
bind = patternBind <|> funBind
  where
    patternBind :: Parser Bind
    patternBind = BindPattern <$> pattern' <* symbol "=" <*> expr

    funBind :: Parser Bind
    funBind = BindFun <$> try (lowerCaseIdent <*> many pattern' <* symbol "->" <*> expr)

operatorTable :: [[Operator Parser LExpr]]
operatorTable =
  [ [prefix "-" (\s e -> Located (Unary (Located UnaryOpNeg s) e) (s <> getLoc e))],
    [ binary "*" (\s l r -> Located (Binary (Located BinaryOpMul s) l r) (getLoc l <> getLoc r)),
      binary "/" (\s l r -> Located (Binary (Located BinaryOpDiv s) l r) (getLoc l <> getLoc r)),
      binary "%" (\s l r -> Located (Binary (Located BinaryOpMod s) l r) (getLoc l <> getLoc r))
    ],
    [ binary "+" (\s l r -> Located (Binary (Located BinaryOpAdd s) l r) (getLoc l <> getLoc r)),
      binary "-" (\s l r -> Located (Binary (Located BinaryOpSub s) l r) (getLoc l <> getLoc r))
    ]
  ]

binary :: Text -> (Loc -> LExpr -> LExpr -> LExpr) -> Operator Parser LExpr
binary name f = InfixL (f . getLoc <$> withLoc (symbol name))

prefix, postfix :: Text -> (Loc -> LExpr -> LExpr) -> Operator Parser LExpr
prefix name f = Prefix (f . getLoc <$> withLoc (string name))
postfix name f = Postfix (f . getLoc <$> withLoc (string name))

typeAnno :: Parser LTypeAnno
typeAnno = arrowType <|> baseType
  where
    arrowType :: Parser LTypeAnno
    arrowType = withLoc $ try (TypeAnnoFun <$> baseType <* symbol "->") <*> typeAnno

    baseType :: Parser LTypeAnno
    baseType =
      withLoc $
        choice
          [ varType,
            identType,
            listType,
            unit $> TypeAnnoUnit
              <|> tupleType
              <|> parens (unLoc <$> typeAnno)
          ]

    varType = TypeAnnoVar <$> lowerCaseIdent
    identType = TypeAnnoIdent <$> upperCaseIdent
    listType = TypeAnnoList <$> brackets typeAnno
    tupleType =
      TypeAnnoTuple
        <$> try (parens ((:) <$> (typeAnno <* char ',')) <*> typeAnno `sepEndBy1` char ',')

pattern' :: Parser LPattern
pattern' = withLoc $ choice [wildcard, litP, identP, consP, listP, unitP]
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

{-# INLINEABLE lowerCaseIdent #-}
lowerCaseIdent :: Parser Ident
lowerCaseIdent = try $ do
  name <- withLoc $ pack <$> ((:) <$> identStartChar <*> many identChar)
  let !sv = unLoc name
  if sv `elem` keywords
    then fail $ "keyword " ++ unpack sv ++ " cannot be used in place of identifier"
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
upperCaseIdent = Ident <$> (withLoc $ pack <$> ((:) <$> upperChar <*> many alphaNumChar))

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

{-# INLINE withLoc #-}
withLoc :: Parser a -> Parser (Located a)
withLoc p = do
  start <- getOffset
  x <- p
  end <- getOffset
  pure $ Located x (Loc start end)