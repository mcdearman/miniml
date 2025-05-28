module MMC.Parser where

import Control.Applicative (empty, optional, (<|>))
import Control.Monad.Combinators.Expr
import Data.Array (Array, listArray)
import Data.Functor (($>))
import Data.Set as Set
import Data.Text (Text, pack, unpack)
import Data.Void
import Data.Word (Word8)
import MMC.AST
import MMC.Common
import MMC.Lexer (WithPos (..))
import MMC.Token (Token (..))
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
  )
import Text.Megaparsec.Debug (MonadParsecDbg (dbg))
import Prelude hiding (span)

type Parser = Parsec Void [WithPos Token]

withSpan :: Parser a -> Parser (Spanned a)
withSpan p = do
  start <- getInput
  x <- p
  end <- getInput
  pure $
    Spanned
      x
      ( case (start, end) of
          ([], _) -> error "Parser started with no input"
          (WithPos _ _ s _ _ : _, WithPos _ _ s' _ _ : _) -> s <> s'
          (WithPos _ _ s _ _ : [], []) -> s
          (WithPos _ _ s _ _ : ts, []) -> Span (spanStart s) (spanEnd (wpSpan (last ts)))
      )

token' :: Token -> Parser Token
token' t = token (\(WithPos _ _ _ _ t') -> if t == t' then Just t else Nothing) Set.empty

int :: Parser (Spanned Integer)
int = token (\case (WithPos _ _ s _ (TokInt n)) -> Just (Spanned n s); _ -> Nothing) Set.empty

bool :: Parser (Spanned Bool)
bool = token (\case (WithPos _ _ s _ (TokBool b)) -> Just (Spanned b s); _ -> Nothing) Set.empty

string :: Parser (Spanned Text)
string = token (\case (WithPos _ _ s _ (TokString str)) -> Just (Spanned str s); _ -> Nothing) Set.empty

unit :: Parser Expr
unit = withSpan $ token' TokLParen *> token' TokRParen $> Unit

lit :: Parser (Spanned Lit)
lit =
  choice
    [ fmap Int <$> int,
      fmap Bool <$> bool,
      fmap String <$> string
    ]

ident :: Parser Ident
ident = token (\case (WithPos _ _ s _ (TokLowerCaseIdent i)) -> Just (Spanned i s); _ -> Nothing) Set.empty

typeIdent :: Parser Ident
typeIdent = token (\case (WithPos _ _ s _ (TokUpperCaseIdent i)) -> Just (Spanned i s); _ -> Nothing) Set.empty

parens :: Parser a -> Parser a
parens = between (token' TokLParen) (token' TokRParen)

brackets :: Parser a -> Parser a
brackets = between (token' TokLBracket) (token' TokRBracket)

-- arrBrackets :: Parser a -> Parser a
-- arrBrackets = between (token' TokHash *> token' TokLBracket) (token' TokRBracket)

braces :: Parser a -> Parser a
braces = between (token' TokLBrace) (token' TokRBrace)

binary :: Token -> (Span -> Expr -> Expr -> Expr) -> Operator Parser Expr
binary tok f = InfixL (f . span <$> withSpan (token' tok))

prefix, postfix :: Token -> (Span -> Expr -> Expr) -> Operator Parser Expr
prefix tok f = Prefix (f . span <$> withSpan (token' tok))
postfix tok f = Postfix (f . span <$> withSpan (token' tok))

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [prefix TokMinus (\s e -> Spanned (Unary (Spanned UnOpNeg s) e) (s <> span e))],
    [ binary TokStar (\s l r -> Spanned (Binary (Spanned BinOpMul s) l r) (span l <> span r)),
      binary TokSlash (\s l r -> Spanned (Binary (Spanned BinOpDiv s) l r) (span l <> span r)),
      binary TokPercent (\s l r -> Spanned (Binary (Spanned BinOpMod s) l r) (span l <> span r))
    ],
    [ binary TokPlus (\s l r -> Spanned (Binary (Spanned BinOpAdd s) l r) (span l <> span r)),
      binary TokMinus (\s l r -> Spanned (Binary (Spanned BinOpSub s) l r) (span l <> span r))
    ]
  ]

pattern' :: Parser Pattern
pattern' = withSpan $ choice [wildcard, litP, varP, listP, unitP]
  where
    wildcard = token' TokUnderscore $> PatternWildcard
    litP = PatternLit . spannedVal <$> lit
    varP = PatternIdent <$> ident
    -- pairP = parens $ PatternPair <$> pattern' <*> (token' TokDoubleColon *> pattern')
    listP = PatternList <$> brackets (pattern' `sepEndBy` token' TokComma)
    unitP = unit $> PatternUnit

type' :: Parser TypeAnno
type' = try arrowType <|> baseType
  where
    arrowType :: Parser TypeAnno
    arrowType = withSpan $ TypeAnnoFun <$> baseType <* token' TokArrow <*> type'

    baseType :: Parser TypeAnno
    baseType =
      withSpan $
        choice
          [ varType,
            identType,
            listType,
            -- recordType,
            try unit $> TypeAnnoUnit
              <|> try tupleType
              <|> parens (spannedVal <$> type')
          ]
    varType = TypeAnnoVar <$> ident
    identType = TypeAnnoIdent <$> typeIdent
    listType = TypeAnnoList <$> brackets type'
    tupleType =
      TypeAnnoTuple
        <$> parens
          ( (:)
              <$> (type' <* token' TokComma)
              <*> type' `sepEndBy1` token' TokComma
          )

-- recordType = do
--   name <- optional typeIdent
--   r <- braces (((,) <$> ident <*> (tokenWithSpan TokColon *> type')) `sepEndBy1` tokenWithSpan TokComma)
--   case name of
--     Nothing -> pure $ Spanned (TypeAnnoRecord Nothing (value r)) (span r)
--     Just n -> pure $ Spanned (TypeAnnoRecord name (value r)) (span n <> span r)

expr :: Parser Expr
expr = makeExprParser apply operatorTable
  where
    unit' = Spanned Unit . span <$> unit
    litExpr = (\l -> Spanned (Lit (spannedVal l)) (span l)) <$> lit
    varExpr = (\i -> Spanned (Var i) (span i)) <$> ident

    simple :: Parser Expr
    simple = withSpan $ choice [litExpr, varExpr, try unit' <|> parens (spannedVal <$> expr)]

    lambda :: Parser ExprSort
    lambda = Lam <$> (token' TokBackSlash *> some pattern' <* token' TokArrow <*> expr)
    -- start <- tokenWithSpan TokBackSlash
    -- ps <- some pattern'
    -- tokenWithSpan TokArrow
    -- e <- expr
    -- pure $ Spanned (Lam ps e) (span start <> span e)

    let' :: Parser Expr
    let' = do
      start <- tokenWithSpan TokLet
      p <- pattern'
      tokenWithSpan TokEq
      e1 <- expr
      tokenWithSpan TokIn
      e2 <- expr
      pure $ Spanned (Let p e1 e2) (span start <> span e2)

    letRec :: Parser Expr
    letRec = do
      start <- tokenWithSpan TokLet
      i <- ident
      ps <- some pattern'
      tokenWithSpan TokEq
      e1 <- expr
      tokenWithSpan TokIn
      e2 <- expr
      pure $ Spanned (Fn i ps e1 e2) (span start <> span e2)

    if' :: Parser Expr
    if' = do
      start <- tokenWithSpan TokIf
      cond <- expr
      tokenWithSpan TokThen
      then' <- expr
      tokenWithSpan TokElse
      else' <- expr
      pure $ Spanned (If cond then' else') (span start <> span else')

    match :: Parser Expr
    match = do
      start <- tokenWithSpan TokMatch
      e <- expr <* tokenWithSpan TokWith <* tokenWithSpan TokBar
      cases <-
        ( ( (,)
              <$> pattern'
              <*> (tokenWithSpan TokArrow *> expr)
          )
        )
          `sepBy1` tokenWithSpan TokBar
      pure $ Spanned (Match e cases) (span start <> span (snd (last cases)))

    list :: Parser Expr
    list = fmap List <$> brackets (expr `sepEndBy` tokenWithSpan TokComma)

    tuple :: Parser Expr
    tuple =
      fmap Tuple
        <$> parens
          ( (:)
              <$> (expr <* tokenWithSpan TokComma)
              <*> expr `sepEndBy1` tokenWithSpan TokComma
          )

    record :: Parser Expr
    record = do
      name <- optional typeIdent
      r <- braces (((,) <$> ident <*> (tokenWithSpan TokEq *> expr)) `sepEndBy1` tokenWithSpan TokComma)
      case name of
        Nothing -> pure $ Spanned (Record Nothing (value r)) (span r)
        Just n -> pure $ Spanned (Record name (value r)) (span n <> span r)

    atom :: Parser Expr
    atom =
      withSpan $
        choice
          [ try letRec <|> let',
            lambda,
            if',
            match,
            list,
            array,
            try tuple <|> simple,
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

parseStream :: [WithPos Token] -> Either (ParseErrorBundle [WithPos Token] Void) Prog
parseStream = Text.Megaparsec.parse repl ""
