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

  $tab                           { \bs -> Token SyntaxKindTab (BL.toStrict bs) }
  $whitespace+                   { \bs -> Token SyntaxKindWhitespace (BL.toStrict bs) }
  "--".*                         { \bs -> Token SyntaxKindComment (BL.toStrict bs) }
  $newline                       { \bs -> Token SyntaxKindNewline (BL.toStrict bs) }

  "("                            { \bs -> Token SyntaxKindLParen (BL.toStrict bs) }
  ")"                            { \bs -> Token SyntaxKindRParen (BL.toStrict bs) }
  "{"                            { \bs -> Token SyntaxKindLBrace (BL.toStrict bs) }
  "}"                            { \bs -> Token SyntaxKindRBrace (BL.toStrict bs) }
  "["                            { \bs -> Token SyntaxKindLBracket (BL.toStrict bs) }
  "]"                            { \bs -> Token SyntaxKindRBracket (BL.toStrict bs) }
  "!"                            { \bs -> Token SyntaxKindBang (BL.toStrict bs) }
  "#"                            { \bs -> Token SyntaxKindHash (BL.toStrict bs) }
  [\\]                           { \bs -> Token SyntaxKindBackSlash (BL.toStrict bs) }
  ":"                            { \bs -> Token SyntaxKindColon (BL.toStrict bs) }
  ";"                            { \bs -> Token SyntaxKindSemi (BL.toStrict bs) }
  ","                            { \bs -> Token SyntaxKindComma (BL.toStrict bs) }
  "."                            { \bs -> Token SyntaxKindPeriod (BL.toStrict bs) }
  "="                            { \bs -> Token SyntaxKindEq (BL.toStrict bs) }
  "<-"                           { \bs -> Token SyntaxKindLArrow (BL.toStrict bs) }
  "->"                           { \bs -> Token SyntaxKindRArrow (BL.toStrict bs) }
  "=>"                           { \bs -> Token SyntaxKindLFatArrow (BL.toStrict bs) }
  "|"                            { \bs -> Token SyntaxKindBar (BL.toStrict bs) }
  "_"                            { \bs -> Token SyntaxKindUnderscore (BL.toStrict bs) }

  @lowerCaseIdent                { \bs -> Token (SyntaxKindLowercaseIdent) (BL.toStrict bs) }
  @upperCaseIdent                { \bs -> Token (SyntaxKindUppercaseIdent) (BL.toStrict bs) }
  @conOpIdent                    { \bs -> Token (SyntaxKindConOpIdent) (BL.toStrict bs) }
  @opIdent                       { \bs -> Token (SyntaxKindOpIdent) (BL.toStrict bs) }

  @int                           { \bs -> Token SyntaxKindInt (BL.toStrict bs) }
  @char                          { \bs -> Token SyntaxKindChar (BL.toStrict bs) }
  @string                        { \bs -> Token SyntaxKindString (BL.toStrict bs) }

  $nonWhite                      { \bs -> Token SyntaxKindError (BL.toStrict bs) }

{
tokenize :: ByteString -> [Token]
tokenize = alexScanTokens
}