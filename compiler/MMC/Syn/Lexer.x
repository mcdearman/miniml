{
module MMC.Syn.Lexer (tokenize) where
import MMC.Syn.GreenNode (Token (..), SyntaxKind (..))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import Data.Text (Text, stripPrefix)
import qualified Data.Text as T
import qualified Data.Char as Char
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

$bareScalar = [^\'\\\n\xD800-\xDFFF]
$escSimple = [0\'\"\\nrtabfv] 

@escByte = \\x ($hexdig | $hexdig $hexdig)

@u4      = $hexdig $hexdig $hexdig $hexdig
@u8      = @u4 @u4
@escUni4 = \\u @u4
@escUni8 = \\U @u8

@lowerCaseIdent = $lower $identChar*
@upperCaseIdent = $upper $identChar*
@opIdent        = $opChar+
@conOpIdent     = ":" $opChar+

@binary      = "0b" $bindig+
@octal       = "0o" $octdig+
@hexadecimal = "0x" $hexdig+
@decimal     = ($nonzero $digit* | "0")
@int         = @binary | @octal | @hexadecimal | @decimal

@char = \' ( $bareScalar | \\$escSimple | @escByte | @escUni4 | @escUni8 ) \'

miniml :-

  $tab                           { \p bs -> Token SyntaxKindTab (BL.toStrict bs) }
  $whitespace+                   { \p bs -> Token SyntaxKindWhitespace (BL.toStrict bs) }
  "--".*                         { \p bs -> Token SyntaxKindComment (BL.toStrict bs) }
  $newline                       { \p bs -> Token SyntaxKindNewline (BL.toStrict bs) }

  "("                            { \p bs -> Token SyntaxKindLParen (BL.toStrict bs) }
  ")"                            { \p bs -> Token SyntaxKindRParen (BL.toStrict bs) }
  "{"                            { \p bs -> Token SyntaxKindLBrace (BL.toStrict bs) }
  "}"                            { \p bs -> Token SyntaxKindRBrace (BL.toStrict bs) }
  "["                            { \p bs -> Token SyntaxKindLBracket (BL.toStrict bs) }
  "]"                            { \p bs -> Token SyntaxKindRBracket (BL.toStrict bs) }
  "!"                            { \p bs -> Token SyntaxKindBang (BL.toStrict bs) }
  "#"                            { \p bs -> Token SyntaxKindHash (BL.toStrict bs) }
  [\\]                           { \p bs -> Token SyntaxKindBackSlash (BL.toStrict bs) }
  ":"                            { \p bs -> Token SyntaxKindColon (BL.toStrict bs) }
  ";"                            { \p bs -> Token SyntaxKindSemi (BL.toStrict bs) }
  ","                            { \p bs -> Token SyntaxKindComma (BL.toStrict bs) }
  "."                            { \p bs -> Token SyntaxKindPeriod (BL.toStrict bs) }
  "="                            { \p bs -> Token SyntaxKindEq (BL.toStrict bs) }
  "<-"                           { \p bs -> Token SyntaxKindLArrow (BL.toStrict bs) }
  "->"                           { \p bs -> Token SyntaxKindRArrow (BL.toStrict bs) }
  "=>"                           { \p bs -> Token SyntaxKindLFatArrow (BL.toStrict bs) }
  "|"                            { \p bs -> Token SyntaxKindBar (BL.toStrict bs) }
  "_"                            { \p bs -> Token SyntaxKindUnderscore (BL.toStrict bs) }

  @lowerCaseIdent                { \p bs -> Token (SyntaxKindLowercaseIdent) (BL.toStrict bs) }
  @upperCaseIdent                { \p bs -> Token (SyntaxKindUppercaseIdent) (BL.toStrict bs) }
  @conOpIdent                    { \p bs -> Token (SyntaxKindConOpIdent) (BL.toStrict bs) }
  @opIdent                       { \p bs -> Token (SyntaxKindOpIdent) (BL.toStrict bs) }

  @int                           { \p bs -> Token SyntaxKindInt (BL.toStrict bs) }
  @char                          { \p bs -> Token SyntaxKindChar (BL.toStrict bs) }

  $nonWhite                      { \p bs -> Token SyntaxKindError (BL.toStrict bs) }

{
tokenize :: ByteString -> [Token]
tokenize = alexScanTokens
}