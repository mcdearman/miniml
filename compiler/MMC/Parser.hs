module MMC.Parser where

import Control.Applicative (empty, optional, (<|>))
import Control.Monad.Combinators.Expr
import Data.Array (Array, listArray)
import Data.Functor (($>))
import qualified Data.Set as Set
import Data.Text (Text, pack, unpack)
import Data.Void
import Data.Word (Word8)
import MMC.AST
import MMC.Common
-- import MMC.Token (Token (..))
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

withSpan :: Parser a -> Parser (Spanned a)
withSpan p = do
  start <- getOffset
  x <- p
  end <- getOffset
  pure $ Spanned x (Span start end)

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

octal :: Parser Integer
octal = try (char '0' *> char' 'o') *> L.octal

hexadecimal :: Parser Integer
hexadecimal = try (char '0' >> char' 'x') >> L.hexadecimal

int :: Parser Integer
int = lexeme $ choice [octal, hexadecimal, L.decimal]

bool :: Parser Bool
bool = lexeme $ choice [True <$ string "true", False <$ string "false"]

str :: Parser Text
str = lexeme $ char '\"' *> (pack <$> manyTill L.charLiteral (char '\"'))

unit :: Parser ()
unit = symbol "()" $> ()

lit :: Parser Lit
lit =
  choice
    [ Int <$> int,
      Bool <$> bool,
      String <$> str
    ]

kwModule :: Parser ()
kwModule = symbol "mod" $> ()

kwImport :: Parser ()
kwImport = symbol "import" $> ()

kwAs :: Parser ()
kwAs = symbol "as" $> ()

kwLet :: Parser ()
kwLet = symbol "let" $> () <?> "let"

kwIn :: Parser ()
kwIn = symbol "in" $> () <?> "in"

kwWhere :: Parser ()
kwWhere = string "where" $> () <?> "where"

kwIf :: Parser ()
kwIf = symbol "if" $> () <?> "if"

kwThen :: Parser ()
kwThen = symbol "then" $> () <?> "then"

kwElse :: Parser ()
kwElse = symbol "else" $> () <?> "else"

kwMatch :: Parser ()
kwMatch = symbol "match" $> () <?> "match"

kwWith :: Parser ()
kwWith = string "with" $> () <?> "with"

kwRecord :: Parser ()
kwRecord = symbol "record" $> () <?> "record"

kwData :: Parser ()
kwData = symbol "data" $> () <?> "data"

kwType :: Parser ()
kwType = symbol "type" $> () <?> "type"

kwClass :: Parser ()
kwClass = symbol "class" $> () <?> "class"

kwInstance :: Parser ()
kwInstance = symbol "impl" $> () <?> "impl"

kwDo :: Parser ()
kwDo = string "do" $> () <?> "do"

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

upperCaseIdent :: Parser Ident
upperCaseIdent = Ident <$> (withSpan $ pack <$> ((:) <$> upperChar <*> many alphaNumChar))

parens :: Parser a -> Parser a
parens = between (lexeme $ char '(') (lexeme $ char ')')

brackets :: Parser a -> Parser a
brackets = between (lexeme $ char '[') (lexeme $ char ']')

braces :: Parser a -> Parser a
braces = between (lexeme $ char '{') (lexeme $ char '}')

binary :: Text -> (Span -> Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f . span <$> withSpan (symbol name))

prefix, postfix :: Text -> (Span -> Expr -> Expr) -> Operator Parser Expr
prefix name f = Prefix (f . span <$> withSpan (string name))
postfix name f = Postfix (f . span <$> withSpan (string name))

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

pattern' :: Parser Pattern
pattern' = withSpan $ choice [wildcard, litP, identP, consP, listP, unitP]
  where
    wildcard = char '_' $> PatternWildcard
    litP = PatternLit <$> lit
    identP = PatternIdent <$> lowerCaseIdent
    consP = PatternCons <$> (upperCaseIdent <* char '@') <*> some pattern'
    listP = PatternList <$> brackets (pattern' `sepEndBy` char ',')
    unitP = unit $> PatternUnit

typeAnno :: Parser TypeAnno
typeAnno = try arrowType <|> baseType
  where
    arrowType :: Parser TypeAnno
    arrowType = withSpan $ TypeAnnoFun <$> baseType <* symbol "->" <*> typeAnno

    baseType :: Parser TypeAnno
    baseType =
      withSpan $
        choice
          [ varType,
            identType,
            listType,
            -- recordType,
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

-- recordType = do
--   name <- optional upperCaseIdent
--   r <- braces (((,) <$> lowerCaseIdent <*> (char ':' *> typeAnno)) `sepEndBy1` char ',')
--   case name of
--     Nothing -> pure $ Spanned (TypeAnnoRecord Nothing (value r)) (span r)
--     Just n -> pure $ Spanned (TypeAnnoRecord name (value r)) (span n <> span r)

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
    match = Match <$> (kwMatch *> expr) <*> indentBlock sc kwWith *> cases
      where
        cases :: Parser [(Pattern, Expr)]
        cases = ((,) <$> pattern' <*> (symbol "->" *> expr)) `sepEndBy1` char ';'

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

-- -- decl :: Parser Decl
-- -- decl = try fnMatch <|> try fn <|> def <|> try record <|> dataDef
-- --   where
-- --     def :: Parser Decl
-- --     def = do
-- --       start <- tokenWithSpan TokDef
-- --       p <- pattern'
-- --       e <- tokenWithSpan TokAssign *> expr
-- --       pure $ Spanned (DeclDef p e) (span start <> span e)

-- --     fn :: Parser Decl
-- --     fn = do
-- --       start <- tokenWithSpan TokDef
-- --       i <- ident
-- --       ps <- some pattern'
-- --       e <- tokenWithSpan TokAssign *> expr
-- --       pure $ Spanned (DeclFn i ps e) (span start <> span e)

-- --     fnMatch :: Parser Decl
-- --     fnMatch = do
-- --       start <- tokenWithSpan TokDef
-- --       i <- ident
-- --       t <- optional (tokenWithSpan TokColon *> type')
-- --       cases <-
-- --         tokenWithSpan TokBar
-- --           *> ( (,)
-- --                  <$> some pattern'
-- --                  <*> (tokenWithSpan TokAssign *> expr)
-- --              )
-- --             `sepEndBy1` tokenWithSpan TokBar
-- --       pure $ Spanned (DeclFnMatch i t cases) (span start <> span (snd (last cases)))

-- --     record :: Parser Decl
-- --     record =
-- --       do
-- --         start <- tokenWithSpan TokData
-- --         name <- typeIdent
-- --         vars <- many tyVar <* tokenWithSpan TokEq
-- --         p <-
-- --           braces
-- --             ( ( (,)
-- --                   <$> ident
-- --                   <*> (tokenWithSpan TokColon *> type')
-- --               )
-- --                 `sepEndBy1` tokenWithSpan TokComma
-- --             )
-- --         pure $ Spanned (DeclRecordDef name vars (value p)) (span start <> span p)

-- --     dataDef :: Parser Decl
-- --     dataDef = do
-- --       name <- tokenWithSpan TokData *> typeIdent
-- --       vars <- many tyVar <* tokenWithSpan TokEq
-- --       p <- ((,) <$> typeIdent <*> many type') `sepEndBy1` tokenWithSpan TokBar
-- --       pure $ Spanned (DeclData name vars p) (span name <> span (last (snd (last p))))

-- dataDef :: Parser DataDef
-- dataDef = do
--   v <- visibility
--   d <- tokenWithSpan TokData
--   name <- typeIdent
--   vars <- many tyVar <* tokenWithSpan TokEq
--   p <- ((,) <$> ident <*> many type') `sepEndBy1` tokenWithSpan TokBar
--   pure $ Spanned (DataDef name vars p v) (span d <> span (last (snd (last p))))

-- repl :: Parser Prog
-- repl = do
--   r <- (try (Right <$> expr) <|> (Left <$> many decl)) <* eof
--   pure $ case r of
--     Left ds ->
--       Spanned
--         (Module (Spanned "main" NoLoc) ds)
--         (case ds of [] -> NoLoc; d : _ -> span d <> span (last ds))
--     Right e ->
--       ( Spanned
--           ( Module
--               (Spanned "main" NoLoc)
--               [Spanned (DeclFn (Spanned "main" $ span e) [] e) (span e)]
--           )
--           (span e)
--       )

repl :: Parser Prog
repl = undefined

parseStream :: Text -> Either (ParseErrorBundle Text Void) Prog
parseStream = Text.Megaparsec.parse repl ""
