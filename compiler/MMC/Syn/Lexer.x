{
module MMC.Syn.Lexer (tokenize) where
import MMC.Syn.Token
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import Data.Text (Text, stripPrefix)
import qualified Data.Text as T
import qualified Data.Char as Char
}

%wrapper "basic-bytestring"

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
@int         = @binary | @octal | @hexadecimal | @decimal

@esc     = \\ ( $escSimple | @escByte | @escUni4 | @escUni8 )
@char    = \' ( $bareScalar | @esc ) \'
@strChar = ( $strBare | $escSimple | @escByte | @escUni4 | @escUni8 )
@string  = \" @strChar* \"

miniml :-

  $tab                           { \bs -> Token TokenKindTab (BL.toStrict bs) }
  $whitespace+                   { \bs -> Token TokenKindWhitespace (BL.toStrict bs) }
  "--".*                         { \bs -> Token TokenKindComment (BL.toStrict bs) }
  $newline                       { \bs -> Token TokenKindNewline (BL.toStrict bs) }

  "("                            { \bs -> Token TokenKindLParen (BL.toStrict bs) }
  ")"                            { \bs -> Token TokenKindRParen (BL.toStrict bs) }
  "{"                            { \bs -> Token TokenKindLBrace (BL.toStrict bs) }
  "}"                            { \bs -> Token TokenKindRBrace (BL.toStrict bs) }
  "["                            { \bs -> Token TokenKindLBracket (BL.toStrict bs) }
  "]"                            { \bs -> Token TokenKindRBracket (BL.toStrict bs) }
  "!"                            { \bs -> Token TokenKindBang (BL.toStrict bs) }
  "#"                            { \bs -> Token TokenKindHash (BL.toStrict bs) }
  [\\]                           { \bs -> Token TokenKindBackSlash (BL.toStrict bs) }
  ":"                            { \bs -> Token TokenKindColon (BL.toStrict bs) }
  ";"                            { \bs -> Token TokenKindSemi (BL.toStrict bs) }
  ","                            { \bs -> Token TokenKindComma (BL.toStrict bs) }
  "."                            { \bs -> Token TokenKindPeriod (BL.toStrict bs) }
  "="                            { \bs -> Token TokenKindEq (BL.toStrict bs) }
  "<-"                           { \bs -> Token TokenKindLArrow (BL.toStrict bs) }
  "->"                           { \bs -> Token TokenKindRArrow (BL.toStrict bs) }
  "=>"                           { \bs -> Token TokenKindLFatArrow (BL.toStrict bs) }
  "|"                            { \bs -> Token TokenKindBar (BL.toStrict bs) }
  "_"                            { \bs -> Token TokenKindUnderscore (BL.toStrict bs) }

  @lowerCaseIdent                { \bs -> Token (TokenKindLowercaseIdent) (BL.toStrict bs) }
  @upperCaseIdent                { \bs -> Token (TokenKindUppercaseIdent) (BL.toStrict bs) }
  @conOpIdent                    { \bs -> Token (TokenKindConOpIdent) (BL.toStrict bs) }
  @opIdent                       { \bs -> Token (TokenKindOpIdent) (BL.toStrict bs) }

  @int                           { \bs -> Token TokenKindInt (BL.toStrict bs) }
  @char                          { \bs -> Token TokenKindChar (BL.toStrict bs) }
  @string                        { \bs -> Token TokenKindString (BL.toStrict bs) }

  $nonWhite                      { \bs -> Token TokenKindError (BL.toStrict bs) }

{
makeInt :: Int -> ByteString -> TokenKind
makeInt 10 bs = TokInt $ parseRadix 10 $ bsToText bs
makeInt 2 bs = TokInt $ parseRadix 2 $ stripIntPrefix bs
makeInt 8 bs = TokInt $ parseRadix 8 $ stripIntPrefix bs
makeInt 16 bs = TokInt $ parseRadix 16 $ stripIntPrefix bs
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