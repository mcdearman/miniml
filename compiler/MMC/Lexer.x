{
module MMC.Lexer (tokenize) where

import MMC.Token
}

%wrapper "basic"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

tokens :-

  $white+                        ;
  "--".*                         ;
  let                            { \s -> TokLet }
  in                             { \s -> TokIn }
  match                         { \s -> TokMatch }
--   $digit+                        { \s -> Int (read s) }
--   [\=\+\-\*\/\(\)]               { \s -> Sym (head s) }
--   $alpha [$alpha $digit \_ \']*  { \s -> Var s }

{

tokenize :: String -> [Token]
tokenize = alexScanTokens
}