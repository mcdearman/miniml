{
module MMC.Lexer (tokenize) where
import MMC.Token
import MMC.Common (Located (..), Loc (..))
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
}

%wrapper "posn-bytestring"

$digit = 0-9      
$alpha = [a-zA-Z]
$lower = [_a-z]
$upper = [A-Z]
$nonWhite = [^$white]

tokens :-

  $white+                        ;
  "--".*                         ;
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

  [\(]                           { \p bs -> Located TokLParen (makeLoc p bs) }
  [\)]                           { \p bs -> Located TokRParen (makeLoc p bs) }
  [\{]                           { \p bs -> Located TokLBrace (makeLoc p bs) }
  [\}]                           { \p bs -> Located TokRBrace (makeLoc p bs) }
  [\[]                           { \p bs -> Located TokLBracket (makeLoc p bs) }
  [\]]                           { \p bs -> Located TokRBracket (makeLoc p bs) }
  [\!]
  [\.]                           { \p bs -> Located TokPeriod (makeLoc p bs) }
  [\,]                           { \p bs -> Located TokComma (makeLoc p bs) }
  [\;]                           { \p bs -> Located TokSemi (makeLoc p bs) }
  [\:]                           { \p bs -> Located TokColon (makeLoc p bs) }
  [\=]                           { \p bs -> Located TokEq (makeLoc p bs) }
--   $digit+                        { \s -> Int (read s) }
--   [\=\+\-\*\/\(\)]               { \s -> Sym (head s) }
--   $alpha [$alpha $digit \_ \']*  { \s -> Var s }
 $nonWhite                      { \p bs -> Located TokError (makeLoc p bs) }

{
makeLoc :: AlexPosn -> ByteString -> Loc
makeLoc (AlexPn start _ _) bs = Loc start end
  where 
    end = start + ((fromIntegral . BL.length) bs)

posnOffset :: AlexPosn -> Int
posnOffset (AlexPn o _ _) = o

tokenize :: ByteString -> [LToken]
tokenize = alexScanTokens
}