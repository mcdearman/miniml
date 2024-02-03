-- Bootstrapped compiler
-- Lexer
type Token = {
  kind : TokenKind,
  span : Span,
}

type TokenKind
  = Eof
  | Whitespace
  | Comment
  | Ident String
  | Int Int
  | Float Float
  | Rational Rational
  | Complex Complex
  | BigInt BigInt
  | BigRational BigRational
  | String String
  | Char Char
  | Plus
  | Minus
  | Star
  | Slash
  | Percent
  | Caret
  | Backslash
  | Arrow
  | FatArrow
  | Pipe
  | PipeArrow
  | Eq
  | Lt
  | Gt
  | Neq
  | Leq
  | Geq
  | LParen
  | RParen
  | LBrack
  | RBrack
  | LBrace
  | RBrace
  | Comma
  | Period
  | Colon
  | Semicolon

type Span = {
  start : Int,
  end : Int,
}

type Lexer = {
  src : String,
  pos : Int,
}

let initLexer src = {
  src = src,
  pos = 0,
}

let isWhitespace c = c = ' ' or c = '\t' or c = '\n' or c = '\r'

let isDigit c = c >= '0' and c <= '9'

let consumeWhitespace lexer =
  let consumeWhitespace' pos =
    if pos < String.length lexer.src && (lexer.src.[pos] == ' ' || lexer.src.[pos] == '\t' || lexer.src.[pos] == '\n') then
      consumeWhitespace' (pos + 1)
    else
      pos
  in
  { lexer with pos = consumeWhitespace' lexer.pos }

let nextToken lexer = 
