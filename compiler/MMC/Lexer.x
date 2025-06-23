{
module MMC.Lexer (tokenize) where
import MMC.Token
}

%wrapper "basic"

$digit = 0-9      
$alpha = [a-zA-Z]
$lower = [_a-z]
$upper = [A-Z]

tokens :-

  $white+                        ;
  "--".*                         ;
  let                            { \s -> TokLet }
  in                             { \s -> TokIn }
--   $digit+                        { \s -> Int (read s) }
--   [\=\+\-\*\/\(\)]               { \s -> Sym (head s) }
--   $alpha [$alpha $digit \_ \']*  { \s -> Var s }

{
tokenize :: String -> [Token]
tokenize = alexScanTokens
}