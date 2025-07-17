{
module MMC.Lexer (tokenize) where
import MMC.Token
import MMC.Common (Located (..), Loc (..))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Data.Char as Char
}

%wrapper "posn-bytestring"

$unispace = \x05
$digit = [0-9]
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

miniml :-

  $whitespace+                   ;
  "--".*                         ;
  $newline                       { \p bs -> Located TokNewline (makeLoc p bs) }
  module                         { \p bs -> Located TokModule (makeLoc p bs) }
  import                         { \p bs -> Located TokImport (makeLoc p bs) }
  as                             { \p bs -> Located TokAs (makeLoc p bs) }
  let                            { \p bs -> Located TokLet (makeLoc p bs) }
  in                             { \p bs -> Located TokIn (makeLoc p bs) }
  where                          { \p bs -> Located TokWhere (makeLoc p bs) }
  if                             { \p bs -> Located TokIf (makeLoc p bs) }
  then                           { \p bs -> Located TokThen (makeLoc p bs) }
  else                           { \p bs -> Located TokElse (makeLoc p bs) }
  match                          { \p bs -> Located TokMatch (makeLoc p bs) }
  with                           { \p bs -> Located TokWith (makeLoc p bs) }
  record                         { \p bs -> Located TokRecord (makeLoc p bs) }
  data                           { \p bs -> Located TokData (makeLoc p bs) }
  type                           { \p bs -> Located TokType (makeLoc p bs) }
  class                          { \p bs -> Located TokClass (makeLoc p bs) }
  instance                       { \p bs -> Located TokInstance (makeLoc p bs) }
  do                             { \p bs -> Located TokDo (makeLoc p bs) }

  "("                            { \p bs -> Located TokLParen (makeLoc p bs) }
  ")"                            { \p bs -> Located TokRParen (makeLoc p bs) }
  "{"                            { \p bs -> Located TokLBrace (makeLoc p bs) }
  "}"                            { \p bs -> Located TokRBrace (makeLoc p bs) }
  "["                            { \p bs -> Located TokLBracket (makeLoc p bs) }
  "]"                            { \p bs -> Located TokRBracket (makeLoc p bs) }
  "!"                            { \p bs -> Located TokBang (makeLoc p bs) }
  [\\]                           { \p bs -> Located TokBackSlash (makeLoc p bs) }
  ":"                            { \p bs -> Located TokColon (makeLoc p bs) }
  ";"                            { \p bs -> Located TokSemi (makeLoc p bs) }
  ","                            { \p bs -> Located TokComma (makeLoc p bs) }
  "."                            { \p bs -> Located TokPeriod (makeLoc p bs) }
  "="                            { \p bs -> Located TokEq (makeLoc p bs) }
  "<-"                           { \p bs -> Located TokLArrow (makeLoc p bs) }
  "->"                           { \p bs -> Located TokRArrow (makeLoc p bs) }
  "=>"                           { \p bs -> Located TokLFatArrow (makeLoc p bs) }
  "|"                            { \p bs -> Located TokBar (makeLoc p bs) }
  "_"                            { \p bs -> Located TokUnderscore (makeLoc p bs) }

  @lowerCaseIdent                { \p bs -> Located (TokLowerCaseIdent ((TE.decodeUtf8 . BL.toStrict) bs)) (makeLoc p bs) }
  @upperCaseIdent                { \p bs -> Located (TokUpperCaseIdent ((TE.decodeUtf8 . BL.toStrict) bs)) (makeLoc p bs) }
  @conOpIdent                    { \p bs -> Located (TokConOpIdent ((TE.decodeUtf8 . BL.toStrict) bs)) (makeLoc p bs) }
  @opIdent                       { \p bs -> Located (TokOpIdent ((TE.decodeUtf8 . BL.toStrict) bs)) (makeLoc p bs) }

--   $digit+                        { \s -> Int (read s) }
--   [\=\+\-\*\/\(\)]               { \s -> Sym (head s) }
--   $alpha [$alpha $digit \_ \']*  { \s -> Var s }
  $nonWhite                       { \p bs -> Located TokError (makeLoc p bs) }

{
-- parseOctal :: ByteString -> Integer
-- parseOctal = foldl' step 0 . BL.unpack
--   where
--     step a c = a * 8 + fromIntegral (Char.digitToInt c) 

makeLoc :: AlexPosn -> ByteString -> Loc
makeLoc (AlexPn start _ _) bs = Loc start end
  where 
    end = start + ((fromIntegral . BL.length) bs)

posnOffset :: AlexPosn -> Int
posnOffset (AlexPn o _ _) = o

tokenize :: ByteString -> [LToken]
tokenize = alexScanTokens
}