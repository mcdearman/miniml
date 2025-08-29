module MMC.Syn.Lexer (tokenize) where

import Control.Applicative (empty, liftA, optional, (<|>))
import Control.Monad (void)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Void (Void)
import Error.Diagnose (Diagnostic)
import qualified FlatParse.Basic as FP
import MMC.Common (Loc (..), Located (..), unLoc)
import MMC.Syn.Token (Token (..), TokenKind (..))

-- type Lexer = Parsec Void Text

tokenize :: Text -> ([Diagnostic Text], [Token])
tokenize = undefined

-- tokenize = (parse . many) tokenL ""

-- tokenKindL :: Lexer TokenKind
-- tokenKindL =
--   choice
--     [ lineComment *> tokenKindL,
--       whitespace,
--       newline,
--       upperCaseIdent,
--       lowerCaseIdent,
--       conOpIdent,
--       opIdent,
--       int,
--       str,
--       charT,
--       TokenKindModule <$ string "module" <?> "module",
--       TokenKindImport <$ string "import" <?> "import",
--       TokenKindAs <$ string "as" <?> "as",
--       TokenKindLet <$ string "let" <?> "let",
--       TokenKindIn <$ string "in" <?> "in",
--       TokenKindWhere <$ string "where" <?> "where",
--       TokenKindIf <$ string "if" <?> "if",
--       TokenKindThen <$ string "then" <?> "then",
--       TokenKindElse <$ string "else" <?> "else",
--       TokenKindMatch <$ string "match" <?> "match",
--       TokenKindWith <$ string "with" <?> "with",
--       TokenKindRecord <$ string "record" <?> "record",
--       TokenKindData <$ string "data" <?> "data",
--       TokenKindType <$ string "type" <?> "type",
--       TokenKindClass <$ string "class" <?> "class",
--       TokenKindInstance <$ string "instance" <?> "instance",
--       TokenKindDo <$ string "do" <?> "do",
--       TokenKindLParen <$ char '(',
--       TokenKindRParen <$ char ')',
--       TokenKindLBrace <$ char '{',
--       TokenKindRBrace <$ char '}',
--       TokenKindLBracket <$ char '[',
--       TokenKindRBracket <$ char ']',
--       TokenKindBang <$ char '!',
--       TokenKindLArrow <$ string "->",
--       TokenKindBackSlash <$ char '\\',
--       TokenKindColon <$ char ':',
--       TokenKindSemi <$ char ';',
--       TokenKindComma <$ char ',',
--       TokenKindPeriod <$ char '.',
--       TokenKindLFatArrow <$ string "=>",
--       TokenKindEq <$ char '=',
--       TokenKindRArrow <$ string "<-",
--       TokenKindBar <$ char '|',
--       TokenKindUnderscore <$ char '_'
--     ]

-- {-# INLINEABLE lowerCaseIdent #-}
-- lowerCaseIdent :: Lexer TokenKind
-- lowerCaseIdent = try $ do
--   name <- pack <$> ((:) <$> identStartChar <*> many identChar)
--   if name `elem` keywords
--     then fail $ "keyword " ++ unpack name ++ " cannot be used in place of identifier"
--     else pure TokenKindLowerCaseIdent
--   where
--     identStartChar = lowerChar <|> char '_'
--     identChar = alphaNumChar <|> char '_' <|> char '\''

--     keywords :: [Text]
--     keywords =
--       [ "module",
--         "import",
--         "as",
--         "let",
--         "in",
--         "where",
--         "if",
--         "then",
--         "else",
--         "match",
--         "with",
--         "record",
--         "data",
--         "type",
--         "class",
--         "instance",
--         "do"
--       ]

-- {-# INLINEABLE upperCaseIdent #-}
-- upperCaseIdent :: Lexer TokenKind
-- upperCaseIdent = TokenKindUpperCaseIdent <$ ((:) <$> upperChar <*> many alphaNumChar)

-- {-# INLINEABLE opIdent #-}
-- opIdent :: Lexer TokenKind
-- opIdent = try $ do
--   sym <- choice [startSpecial, startNotEq] <* notFollowedBy (oneOf ['=', '.', '@', '|', ':', '-'])
--   if sym `elem` reservedSymbols
--     then fail $ "symbol " ++ unpack sym ++ " cannot be used in place of identifier"
--     else pure TokenKindOpIdent
--   where
--     opStartChar = oneOf ("!$%&*+/<>?~" ++ ['^' .. '`'] :: String)
--     startSpecial = try $ T.cons <$> oneOf ['=', '.', '@', '|', ':'] <*> takeWhile1P Nothing isOpChar
--     startNotEq = T.cons <$> opStartChar <*> takeWhileP Nothing isOpChar

--     reservedSymbols :: [Text]
--     reservedSymbols =
--       [ "->",
--         "=>",
--         "<-",
--         "!"
--       ]

-- {-# INLINEABLE conOpIdent #-}
-- conOpIdent :: Lexer TokenKind
-- conOpIdent = try $ TokenKindConOpIdent <$ (T.cons <$> char ':' <*> takeWhile1P Nothing isOpChar)

-- {-# INLINEABLE isOpChar #-}
-- isOpChar :: Char -> Bool
-- isOpChar c = c `elem` ("!$%&*+./<=>?@|\\~:" ++ ['^' .. '`'] :: String)

-- newline :: Lexer TokenKind
-- newline = TokenKindNewline <$ oneOf ['\n', '\r']

-- whitespace :: Lexer TokenKind
-- whitespace = TokenKindWhitespace <$ takeWhile1P (Just "whitespace") isSpace
--   where
--     isSpace c = c == ' ' || c == '\t'

-- lineComment :: Lexer ()
-- lineComment = L.skipLineComment "--"

-- {-# INLINE octal #-}
-- octal :: Lexer Integer
-- octal = try $ char '0' *> char' 'o' *> L.octal

-- {-# INLINE hexadecimal #-}
-- hexadecimal :: Lexer Integer
-- hexadecimal = try $ char '0' *> char' 'x' *> L.hexadecimal

-- {-# INLINE int #-}
-- int :: Lexer TokenKind
-- int = TokenKindInt <$ choice [octal, hexadecimal, L.decimal]

-- {-# INLINE str #-}
-- str :: Lexer TokenKind
-- str = TokenKindString <$ (char '\"' *> (pack <$> manyTill L.charLiteral (char '\"')))

-- {-# INLINE charT #-}
-- charT :: Lexer TokenKind
-- charT = TokenKindChar <$ (char '\'' *> L.charLiteral <* char '\'')

-- {-# INLINE tokenL #-}
-- tokenL :: Lexer Token
-- tokenL = do
--   start <- getOffset
--   kind <- tokenKindL
--   Token kind . Loc start <$> getOffset