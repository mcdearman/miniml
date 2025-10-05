{
module MMC.Syn.Lexer (tokenize) where
import MMC.Syn.Token
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import Data.Text (Text, stripPrefix)
import qualified Data.Text as T
import qualified Data.Char as Char
import MMC.Utils.Span
}

%wrapper "posn-bytestring"

$unispace   = \x05
$nonzero    = [1-9]
$digit      = [0-9]
$bindig     = [01]
$octdig     = [0-7]
$hexdig     = [0-9A-Fa-f]      
$alpha      = [a-zA-Z]
$lower      = [_a-z]
$upper      = [A-Z]
$nonWhite   = [^$white]
$tab        = \t
$whitespace = [\ $unispace\v]
$newline    = [\n\r\f]
$identChar  = [$alpha $digit \_ \']
$opChar     = [\!\$\%\&\*\+\.\/\<\=\>\?\@\|\\\~\:\^-\`]

$strBare    = [^\"\\\n\xD800-\xDFFF]
$bareScalar = [^\'\\\n\xD800-\xDFFF]
$escSimple = [0\'\"\\nrtabfv] 

@escByte = x ($hexdig | $hexdig $hexdig)

@u4      = $hexdig $hexdig $hexdig $hexdig
@u8      = @u4 @u4
@escUni4 = u @u4
@escUni8 = U @u8

@lowerCaseIdent = $lower $identChar*
@upperCaseIdent = $upper $identChar*
@opIdent        = $opChar+
@conOpIdent     = ":" $opChar+

@binary      = "0b" $bindig+
@octal       = "0o" $octdig+
@hexadecimal = "0x" $hexdig+
@decimal     = ($nonzero $digit* | "0")
-- @int         = @binary | @octal | @hexadecimal | @decimal

@esc     = \\ ( $escSimple | @escByte | @escUni4 | @escUni8 )
@char    = \' ( $bareScalar | @esc ) \'
@strChar = ( $strBare | $escSimple | @escByte | @escUni4 | @escUni8 )
@string  = \" @strChar* \"

miniml :-

  $tab                           { \p bs -> Token TokenKindTab (makeSpan p bs) }
  $whitespace+                   { \p bs -> Token TokenKindWhitespace (makeSpan p bs) }
  "--".*                         { \p bs -> Token TokenKindComment (makeSpan p bs) }
  $newline                       { \p bs -> Token TokenKindNewline (makeSpan p bs) }

  "("                            { \p bs -> Token TokenKindLParen (makeSpan p bs) }
  ")"                            { \p bs -> Token TokenKindRParen (makeSpan p bs) }
  "{"                            { \p bs -> Token TokenKindLBrace (makeSpan p bs) }
  "}"                            { \p bs -> Token TokenKindRBrace (makeSpan p bs) }
  "["                            { \p bs -> Token TokenKindLBracket (makeSpan p bs) }
  "]"                            { \p bs -> Token TokenKindRBracket (makeSpan p bs) }
  "!"                            { \p bs -> Token TokenKindBang (makeSpan p bs) }
  "#"                            { \p bs -> Token TokenKindHash (makeSpan p bs) }
  [\\]                           { \p bs -> Token TokenKindBackSlash (makeSpan p bs) }
  ":"                            { \p bs -> Token TokenKindColon (makeSpan p bs) }
  ";"                            { \p bs -> Token TokenKindSemi (makeSpan p bs) }
  ","                            { \p bs -> Token TokenKindComma (makeSpan p bs) }
  "."                            { \p bs -> Token TokenKindPeriod (makeSpan p bs) }
  "="                            { \p bs -> Token TokenKindEq (makeSpan p bs) }
  "<-"                           { \p bs -> Token TokenKindLArrow (makeSpan p bs) }
  "->"                           { \p bs -> Token TokenKindRArrow (makeSpan p bs) }
  "=>"                           { \p bs -> Token TokenKindLFatArrow (makeSpan p bs) }
  "|"                            { \p bs -> Token TokenKindBar (makeSpan p bs) }
  "_"                            { \p bs -> Token TokenKindUnderscore (makeSpan p bs) }

  @lowerCaseIdent                { \p bs -> Token (TokenKindLowercaseIdent $ BL.toStrict bs) (makeSpan p bs) }
  @upperCaseIdent                { \p bs -> Token (TokenKindUppercaseIdent $ BL.toStrict bs) (makeSpan p bs) }
  @conOpIdent                    { \p bs -> Token (TokenKindConOpIdent $ BL.toStrict bs) (makeSpan p bs) }
  @opIdent                       { \p bs -> Token (TokenKindOpIdent $ BL.toStrict bs) (makeSpan p bs) }

  @decimal                       { \p bs -> Token (makeInt 10 bs) (makeSpan p bs) }
  @binary                        { \p bs -> Token (makeInt 2 bs) (makeSpan p bs) }
  @octal                         { \p bs -> Token (makeInt 8 bs) (makeSpan p bs) }
  @hexadecimal                   { \p bs -> Token (makeInt 16 bs) (makeSpan p bs) }
  @char                          { \p bs -> Token (TokenKindChar $ bToChar bs) (makeSpan p bs) }
  @string                        { \p bs -> Token (TokenKindString $ BL.toStrict bs) (makeSpan p bs) }

  $nonWhite                      { \p bs -> Token TokenKindError (makeSpan p bs) }

{
makeInt :: Int -> ByteString -> TokenKind
makeInt 10 bs = TokenKindInt $ parseRadix 10 $ bsToText bs
makeInt 2 bs = TokenKindInt $ parseRadix 2 $ stripIntPrefix bs
makeInt 8 bs = TokenKindInt $ parseRadix 8 $ stripIntPrefix bs
makeInt 16 bs = TokenKindInt $ parseRadix 16 $ stripIntPrefix bs
makeInt r _ = error $ "Unsupported radix" ++ show r

{-# INLINE stripIntPrefix #-}
stripIntPrefix :: ByteString -> Text
stripIntPrefix bs = T.drop 2 $ bsToText bs

{-# INLINE bsToText #-}
bsToText :: ByteString -> Text
bsToText = TE.decodeUtf8 . BL.toStrict

{-# INLINE bToChar #-}
bToChar :: ByteString -> Char
bToChar = Char.chr . fromIntegral . BL.head

parseRadix :: (Integral a) => a -> Text -> a
parseRadix r = T.foldl' step 0
  where
    step a c = a * r + (fromIntegral $ Char.digitToInt c)

makeSpan :: AlexPosn -> ByteString -> Span
makeSpan (AlexPn start _ _) bs = Span start end
  where 
    end = start + (fromIntegral $ BL.length bs)

posnOffset :: AlexPosn -> Int
posnOffset (AlexPn o _ _) = o

tokenize :: ByteString -> [Token]
tokenize = alexScanTokens
}