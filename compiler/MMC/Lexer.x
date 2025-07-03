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
$nonWhite = \S

tokens :-

  $white+                        ;
  "--".*                         ;
  let                            { \p bs -> Located TokLet (makeLoc p bs) }
  in                             { \p bs -> Located TokIn (makeLoc p bs) }
--   $digit+                        { \s -> Int (read s) }
--   [\=\+\-\*\/\(\)]               { \s -> Sym (head s) }
--   $alpha [$alpha $digit \_ \']*  { \s -> Var s }
 $nonWhite+                      { \p bs -> Located TokError (makeLoc p bs) }

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