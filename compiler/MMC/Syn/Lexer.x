{
module MMC.Syn.Lexer (tokenize) where
import MMC.Syn.Token
import MMC.Common (Loc (..))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import Data.Text (Text, stripPrefix)
import qualified Data.Text as T
import qualified Data.Char as Char
}

%wrapper "posn-bytestring"

$unispace = \x05
$nonzero = [1-9]
$digit = [0-9]
$bindig = [01]
$octdig = [0-7]
$hexdig = [0-9A-Fa-f]      
$alpha = [a-zA-Z]
$lower = [_a-z]
$upper = [A-Z]
$nonWhite = [^$white]
$whitespace = [\ $unispace\t\v]
$newline = [\n\r\f]
$identChar = [$alpha $digit \_ \']
$opChar = [\!\$\%\&\*\+\.\/\<\=\>\?\@\|\\\~\:\^-\`]

@lowerCaseIdent = $lower $identChar*
@upperCaseIdent = $upper $identChar*
@opIdent = $opChar+
@conOpIdent = ":" $opChar+

@binary = "0b" $bindig+
@octal = "0o" $octdig+
@hexadecimal = "0x" $hexdig+
@decimal = ($nonzero $digit* | "0")

miniml :-

  $whitespace+                   ;
  "--".*                         ;
  $newline                       { \p bs -> Token TokenKindNewline (makeLoc p bs) }
  module                         { \p bs -> Token TokenKindModule (makeLoc p bs) }
  import                         { \p bs -> Token TokenKindImport (makeLoc p bs) }
  as                             { \p bs -> Token TokenKindAs (makeLoc p bs) }
  let                            { \p bs -> Token TokenKindLet (makeLoc p bs) }
  in                             { \p bs -> Token TokenKindIn (makeLoc p bs) }
  where                          { \p bs -> Token TokenKindWhere (makeLoc p bs) }
  if                             { \p bs -> Token TokenKindIf (makeLoc p bs) }
  then                           { \p bs -> Token TokenKindThen (makeLoc p bs) }
  else                           { \p bs -> Token TokenKindElse (makeLoc p bs) }
  match                          { \p bs -> Token TokenKindMatch (makeLoc p bs) }
  with                           { \p bs -> Token TokenKindWith (makeLoc p bs) }
  record                         { \p bs -> Token TokenKindRecord (makeLoc p bs) }
  data                           { \p bs -> Token TokenKindData (makeLoc p bs) }
  type                           { \p bs -> Token TokenKindType (makeLoc p bs) }
  class                          { \p bs -> Token TokenKindClass (makeLoc p bs) }
  instance                       { \p bs -> Token TokenKindInstance (makeLoc p bs) }
  do                             { \p bs -> Token TokenKindDo (makeLoc p bs) }

  "("                            { \p bs -> Token TokenKindLParen (makeLoc p bs) }
  ")"                            { \p bs -> Token TokenKindRParen (makeLoc p bs) }
  "{"                            { \p bs -> Token TokenKindLBrace (makeLoc p bs) }
  "}"                            { \p bs -> Token TokenKindRBrace (makeLoc p bs) }
  "["                            { \p bs -> Token TokenKindLBracket (makeLoc p bs) }
  "]"                            { \p bs -> Token TokenKindRBracket (makeLoc p bs) }
  "!"                            { \p bs -> Token TokenKindBang (makeLoc p bs) }
  [\\]                           { \p bs -> Token TokenKindBackSlash (makeLoc p bs) }
  ":"                            { \p bs -> Token TokenKindColon (makeLoc p bs) }
  ";"                            { \p bs -> Token TokenKindSemi (makeLoc p bs) }
  ","                            { \p bs -> Token TokenKindComma (makeLoc p bs) }
  "."                            { \p bs -> Token TokenKindPeriod (makeLoc p bs) }
  "="                            { \p bs -> Token TokenKindEq (makeLoc p bs) }
  "<-"                           { \p bs -> Token TokenKindLArrow (makeLoc p bs) }
  "->"                           { \p bs -> Token TokenKindRArrow (makeLoc p bs) }
  "=>"                           { \p bs -> Token TokenKindLFatArrow (makeLoc p bs) }
  "|"                            { \p bs -> Token TokenKindBar (makeLoc p bs) }
  "_"                            { \p bs -> Token TokenKindUnderscore (makeLoc p bs) }

  @lowerCaseIdent                { \p bs -> Token (TokenKindLowerCaseIdent <$ bsToText bs) (makeLoc p bs) }
  @upperCaseIdent                { \p bs -> Token (TokenKindUpperCaseIdent <$ bsToText bs) (makeLoc p bs) }
  @conOpIdent                    { \p bs -> Token (TokenKindConOpIdent <$ bsToText bs) (makeLoc p bs) }
  @opIdent                       { \p bs -> Token (TokenKindOpIdent <$ bsToText bs) (makeLoc p bs) }

  @decimal                       { \p bs -> Token (makeInt 10 bs) (makeLoc p bs) }
  @binary                        { \p bs -> Token (makeInt 2 bs) (makeLoc p bs) }
  @octal                         { \p bs -> Token (makeInt 8 bs) (makeLoc p bs) }
  @hexadecimal                   { \p bs -> Token (makeInt 16 bs) (makeLoc p bs) }

  $nonWhite                      { \p bs -> Token TokenKindError (makeLoc p bs) }

{
makeInt :: Int -> ByteString -> Token
makeInt 10 bs = TokenKindInt <$ parseRadix 10 $ bsToText bs
makeInt 2 bs = TokenKindInt <$ parseRadix 2 $ stripIntPrefix bs
makeInt 8 bs = TokenKindInt <$ parseRadix 8 $ stripIntPrefix bs
makeInt 16 bs = TokenKindInt <$ parseRadix 16 $ stripIntPrefix bs
makeInt r _ = error $ "Unsupported radix" ++ show r

{-# INLINE stripIntPrefix #-}
stripIntPrefix :: ByteString -> Text
stripIntPrefix bs = T.drop 2 $ bsToText bs

{-# INLINE bsToText #-}
bsToText :: ByteString -> Text
bsToText = TE.decodeUtf8 . BL.toStrict

parseRadix :: (Integral a) => a -> Text -> a
parseRadix r = T.foldl' step 0
  where
    step a c = a * r + (fromIntegral $ Char.digitToInt c)

makeLoc :: AlexPosn -> ByteString -> Loc
makeLoc (AlexPn start _ _) bs = Loc start end
  where 
    end = start + (fromIntegral $ BL.length bs)

posnOffset :: AlexPosn -> Int
posnOffset (AlexPn o _ _) = o

tokenize :: ByteString -> [Token]
tokenize = alexScanTokens
}