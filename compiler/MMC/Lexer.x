{
module MMC.Lexer (tokenize) where
import MMC.Token
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

tokens :-

  $white+                        ;
  "--".*                         ;
  let                            { \p bs -> TokLet }
  in                             { \p bs -> TokIn }
--   $digit+                        { \s -> Int (read s) }
--   [\=\+\-\*\/\(\)]               { \s -> Sym (head s) }
--   $alpha [$alpha $digit \_ \']*  { \s -> Var s }
  . .                            { \p bs -> TokError ()  }

{
tokenize :: ByteString -> [Token]
tokenize = alexScanTokens
}