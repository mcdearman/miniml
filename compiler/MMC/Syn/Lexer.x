{
module MMC.Syn.Lexer (tokenize) where
import MMC.Syn.Token
import MMC.Utils.Span (Span (..))
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
$tab = \t
$whitespace = [\ $unispace\v]
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
@int = @binary | @octal | @hexadecimal | @decimal

miniml :-

  $tab                           { \p bs -> Token TokenKindTab (BL.toStrict bs) }
  $whitespace+                   { \p bs -> Token TokenKindWhitespace (BL.toStrict bs) }
  "--".*                         { \p bs -> Token TokenKindComment (BL.toStrict bs) }
  $newline                       { \p bs -> Token TokenKindNewline (BL.toStrict bs) }

  "("                            { \p bs -> Token TokenKindLParen (BL.toStrict bs) }
  ")"                            { \p bs -> Token TokenKindRParen (BL.toStrict bs) }
  "{"                            { \p bs -> Token TokenKindLBrace (BL.toStrict bs) }
  "}"                            { \p bs -> Token TokenKindRBrace (BL.toStrict bs) }
  "["                            { \p bs -> Token TokenKindLBracket (BL.toStrict bs) }
  "]"                            { \p bs -> Token TokenKindRBracket (BL.toStrict bs) }
  "!"                            { \p bs -> Token TokenKindBang (BL.toStrict bs) }
  "#"                            { \p bs -> Token TokenKindHash (BL.toStrict bs) }
  [\\]                           { \p bs -> Token TokenKindBackSlash (BL.toStrict bs) }
  ":"                            { \p bs -> Token TokenKindColon (BL.toStrict bs) }
  ";"                            { \p bs -> Token TokenKindSemi (BL.toStrict bs) }
  ","                            { \p bs -> Token TokenKindComma (BL.toStrict bs) }
  "."                            { \p bs -> Token TokenKindPeriod (BL.toStrict bs) }
  "="                            { \p bs -> Token TokenKindEq (BL.toStrict bs) }
  "<-"                           { \p bs -> Token TokenKindLArrow (BL.toStrict bs) }
  "->"                           { \p bs -> Token TokenKindRArrow (BL.toStrict bs) }
  "=>"                           { \p bs -> Token TokenKindLFatArrow (BL.toStrict bs) }
  "|"                            { \p bs -> Token TokenKindBar (BL.toStrict bs) }
  "_"                            { \p bs -> Token TokenKindUnderscore (BL.toStrict bs) }

  @lowerCaseIdent                { \p bs -> Token (TokenKindLowercaseIdent) (BL.toStrict bs) }
  @upperCaseIdent                { \p bs -> Token (TokenKindUppercaseIdent) (BL.toStrict bs) }
  @conOpIdent                    { \p bs -> Token (TokenKindConOpIdent) (BL.toStrict bs) }
  @opIdent                       { \p bs -> Token (TokenKindOpIdent) (BL.toStrict bs) }

  @int                           { \p bs -> Token TokenKindInt (BL.toStrict bs) }

  $nonWhite                      { \p bs -> Token TokenKindError (BL.toStrict bs) }

{
tokenize :: ByteString -> [Token]
tokenize = alexScanTokens
}